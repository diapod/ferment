# Ferment – Design

## 1. Cel systemu

Ferment to silnik agenta AI rozwijany iteracyjnie, przez pączkowanie:
- najpierw mały, działający rdzeń,
- potem dokładanie nowych „pączków” funkcji jako niezależnych klocków,
- bez przepisywania całego systemu.

Główny cel:
- utrzymać prosty kontrakt danych,
- mieć deterministyczny pipeline orkiestracji,
- rozdzielić warstwy: domena → orkiestracja → adaptery → runtime.

## 2. Źródła prawdy operacyjnej

Ten dokument i `doc/stratification.md` są wspólnie źródłami prawdy o systemie:
- `doc/design.md` definiuje architekturę, moduły i plan rozwoju,
- `doc/stratification.md` definiuje semantykę wykonania wywołań (capabilities, IR, aplikator jakości).

Reguła spójności:
- dla semantyki wywołań i delegacji obowiązuje model z `doc/stratification.md`,
- dla podziału modułów i etapów rozwoju obowiązuje model z `doc/design.md`.

## 3. Zasady architektoniczne

1. Dane przed obiektami: przepływ oparty o mapy EDN i jawne klucze.
2. Chude interfejsy: funkcje na granicach modułów przyjmują i zwracają mapy.
3. Separacja efektów ubocznych: I/O tylko w adapterach.
4. Determinizm gdzie możliwe: ten sam input ⇒ ten sam plan wykonania.
5. Pączkowanie: nowa funkcja trafia do nowego modułu i jest „wpinana” przez registry/router.
6. Wszystko jest capability: LLM i non-LLM są wywoływane przez ten sam kontrakt.
7. Delegacja jest late-bound i quality-aware: wybór wykonawcy może się zmienić na etapie aplikatora.

## 4. Semantyka wywołań (integracja stratyfikacji)

### 4.1. Jedna konwencja wywołania
`invoke(cap, ctx, req) -> result`

- `cap`: capability-handle (`:cap/id` + meta), nie „sztywna nazwa modelu”,
- `ctx`: kontekst wykonania (trace, budżet, call stack, polityki),
- `req`: request strukturalny, nie goły prompt.

### 4.2. Program jako dane (IR)
Plan wykonania reprezentujemy jako IR/AST (EDN) z węzłami wywołań.

Minimalny kontrakt wyników capability:
- `Value` – gotowa wartość,
- `Plan` – kolejny plan do wykonania (rekurencyjna delegacja),
- `Stream` – opcjonalnie wyniki przyrostowe.

### 4.3. Aplikator jakości
Runtime działa jak evaluator:
- rozpoznaje węzły wywołań,
- rozwiązuje wykonawcę (late binding),
- uruchamia, waliduje i komituje wynik,
- wykonuje fallback/race/repair-loop zgodnie z politykami jakości.

### 4.4. Delegacja deklaratywna
W planie zapisujemy:
- `:intent`,
- `:requires` (kontrakty, schemat wyjścia),
- `:dispatch` (kandydaci, polityki),
a nie „użyj modelu X na sztywno”.

## 5. Warstwy systemu

### 5.1. Warstwa domenowa (pure)
- klasyfikacja intencji,
- budowa planu odpowiedzi,
- routing do roli/capability,
- składanie promptu.

Wymaganie: bez I/O i bez zależności od konkretnego dostawcy.

### 5.2. Warstwa orkiestracji (workflow)
- sekwencja kroków (pipeline),
- polityki retry/timeout/fallback,
- sterowanie budżetem i jakością.

Wymaganie: logika sterowania bez bezpośredniego `curl`/procesów.

### 5.3. Warstwa adapterów (integration)
- adaptery LLM (Ollama, MLX, docelowo OpenAI),
- adaptery non-LLM (narzędzia systemowe, solvery),
- adapter konfiguracji i środowiska.

Wymaganie: wszystkie efekty uboczne i mapowanie protokołów są tutaj.

### 5.4. Warstwa runtime (entrypoints)
- CLI (`-main`),
- tryb batch/serwer,
- bootstrap konfiguracji (`ferment.system/read-configs`).

Wymaganie: cienka warstwa spinająca pozostałe.

## 6. Mapa modułów (obecna i docelowa)

Obecne klocki:
- `ferment.core` – podstawowy pipeline rozmowy i wywołania Ollama,
- `ferment.mlx` – adapter HTTP dla MLX + wrapper chat/generate,
- `ferment.system`, `ferment.env`, `ferment.env.file`, `ferment.readers` – konfiguracja i lifecycle usług,
- `ferment.utils*` – funkcje pomocnicze.

Docelowe pączki:
- `ferment.protocols` – kontrakty capability i runtime,
- `ferment.router` – routing intencji i polityk jakości,
- `ferment.workflow` – wykonanie planu kroków (IR eval/apply),
- `ferment.adapters.*` – dostawcy modeli i narzędzia,
- `ferment.memory` – stan sesji, kontekst, cache,
- `ferment.telemetry` – logi strukturalne, metryki, trace-id.

### 6.1. Stan implementacji (2026-02-15)

W systemie działa już podział na gałęzie konfiguracyjne Integranta:
- `:ferment.runtime/default` – runtime input dla orkiestratora (`config` z wartościami env + refs do resolver/protocol),
- `:ferment.core/default` – usługa core (`:solver!`, `:voice!`, `:respond!`) inicjalizowana z runtime.
- `:ferment.model/*` – selektory modeli (`profile`, `solver`, `voice`) rozwiązywane na etapie konfiguracji.

Aktualny bootstrap:
- produkcyjny: `ferment.app/start!` (ładuje `resources/config/common/prod` + `resources/config/local/prod`),
- developerski: `ferment.app/start-dev!` (ładuje `prod` i następnie nakłada `dev`; `dev` nadpisuje gałęzie),
- administracyjny: `ferment.app/start-admin!` (ładuje `prod` i następnie nakłada `admin`).

Konwencja profili:
- profile aplikacji: `:prod`, `:dev`, `:admin`,
- zmienna `FERMENT_PROFILE` jest traktowana pomocniczo (np. dla skryptów shellowych), a profil runtime pozostaje również dostępny w `:ferment.app/properties`.

Cel praktyczny:
- utrzymać czytelny, warstwowy model konfiguracji (`common` + `local`, `prod` + overlay środowiskowy) bez rozjazdu między konfiguracją aplikacji i narzędziami shellowymi.

## 7. Kontrakty danych (v1)

### 7.1. Wejście
```clojure
{:session/id "..."
 :request/id "..."
 :messages   [{:role "user" :content "..."}]
 :context    {:lang "pl" :mode :chat}
 :opts       {:temperature 0.4 :max-tokens 512}}
```

### 7.2. Plan orkiestracji
```clojure
{:request/id "..."
 :intent     :coding|:chat|:analysis
 :requires   {:out/schema :res/patch+tests
              :quality/must [:schema-valid :tests-pass]}
 :dispatch   {:candidates [:llm/coder-a :llm/coder-b :solver/non-llm-x]
              :policy     :quality-aware}
 :steps      [{:op :build-prompt}
              {:op :call-capability}
              {:op :postprocess}]}
```

### 7.3. Odpowiedź
```clojure
{:request/id "..."
 :status     :ok|:error
 :output     {:text "..."}
 :usage      {:latency-ms 1234}
 :quality    {:must-pass? true :score 0.91}
 :errors     []}
```

## 8. Podstawowe funkcje (MVP)

1. `classify-intent` – rozpoznaje typ zapytania.
2. `build-plan` – składa plan kroków na podstawie intentu.
3. `resolve-capability` – wybiera kandydata według polityki.
4. `execute-plan` – wykonuje kroki planu przez aplikator.
5. `call-capability` – zunifikowany punkt wywołania capability.
6. `respond` – zwraca finalną odpowiedź + metadane jakości.

## 9. Plan pączkowania (etapy)

### Etap A – Stabilny rdzeń
- wydzielić czysty pipeline do `ferment.workflow`,
- unifikować shape danych (`request`, `plan`, `response`),
- dodać testy kontraktowe dla pipeline.

### Etap B – Router i role
- przenieść heurystykę do `ferment.router`,
- dodać profile roli (solver/voice),
- odseparować prompt-building od adaptera modelu.

### Etap C – Capability runtime
- wprowadzić registry capabilities i `invoke(cap, ctx, req)`,
- obsłużyć `Value | Plan | Stream`,
- dodać fallback/race/retry i bramki jakości.

### Etap D – Pamięć i obserwowalność
- `session-state` i korelacja `request/id`,
- telemetry events (start/stop/error),
- lokalny cache odpowiedzi (opcjonalnie).

## 10. Spójność z `doc/stratification.md` (brak kolizji)

Nie wykryto kolizji semantycznych; dokumenty są komplementarne.

Zdolności, które pozostają zachowane:
1. heterogeniczne wykonawstwo (LLM + non-LLM) przez capability,
2. dowolna głębokość delegacji (A→B→C→…),
3. late binding wykonawcy na etapie runtime,
4. quality-aware dispatch (nie tylko availability/latency),
5. reprezentacja planu jako IR/AST i ewaluacja przez aplikator.

## 11. Mapa zgodności 1:1 (`stratification` → `design`)

| `doc/stratification.md` | `doc/design.md` | Status |
|---|---|---|
| 1.1 „Wszystko jest capability” | 3.6, 4.1, 6, 8.5 | Pokryte |
| 1.2 `invoke(cap, ctx, req) -> result` | 4.1, 8.5, 9.C | Pokryte |
| 1.3 IR jako AST/Plan | 4.2, 6 (`ferment.workflow`), 9.C | Pokryte |
| 1.4 `Value \| Plan \| Stream` | 4.2, 9.C | Pokryte |
| 1.5 Aplikator jako evaluator | 4.3, 8.4, 9.C | Pokryte |
| 1.6 Delegacja deklaratywna (`intent` + `requires` + `dispatch`) | 4.4, 7.2 | Pokryte |
| 2.1 Uniformizacja przez schemat i kontrakty jakości | 7.2, 7.3, 10 | Pokryte |
| 2.2 HOF/capability jako wartość | 3.6, 4.2, 8.3 | Pokryte |
| 2.3 Late binding/lazy runtime jako resilience | 3.7, 4.3, 9.C | Pokryte |
| 2.4 Quality-aware dispatch | 3.7, 4.3, 7.2, 10 | Pokryte |
| 3. Minimalne schematy EDN (registry/dispatch/wynik) | 7.2, 7.3, 9.C | Częściowo pokryte (struktura tak, pełne pola registry do uszczegółowienia) |

Uwagi integracyjne:
1. Brak kolizji semantycznych; oba dokumenty są komplementarne.
2. Nie tracimy zdolności ze stratyfikacji: heterogeniczne wykonawstwo, rekurencyjna delegacja, late binding i quality-aware dispatch pozostają wymaganiami architektury.
3. Do doprecyzowania implementacyjnego w Etapie C: komplet pól metadata capability (np. `:cap/cost`, `:cap/limits`) oraz strojenie polityk jakości per-intent (`:done`, `:switch-on`, scoring judge).

## 12. Definicja „done” dla nowych pączków

Każdy nowy pączek jest „done”, gdy:
1. ma jawny kontrakt wejścia/wyjścia,
2. ma test jednostkowy kontraktu,
3. nie rozszerza odpowiedzialności istniejących modułów,
4. da się włączyć/wyłączyć przez konfigurację,
5. nie łamie reguł z `doc/stratification.md`.

---

Dokument startowy v0.3. Każda iteracja aktualizuje:
- kontrakty danych,
- mapę modułów,
- plan etapów,
- inwarianty jakości i reguły dispatch.
