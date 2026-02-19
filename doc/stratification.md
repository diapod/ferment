# Stratyfikacja wywołań (LLM i non-LLM) przez capabilities + IR + aplikator jakości

Poniższe założenia i wnioski opisują architekturę „stratified design” dla systemu wieloagentowego, w którym:
- jednostki wykonawcze mogą być LLM-ami lub solverami nie-LLM,
- wywołania mogą się zagnieżdżać dowolnie głęboko (A→B→C→…),
- delegacja jest *late-bound* (lazy aplikator może zmienić wykonawcę, gdy adres/capability jest niedostępny),
- routing jest **quality-aware** (fallback również na podstawie jakości, nie tylko dostępności/latencji).

## 1) Kluczowe założenia

### 1.1. Abstrakcja: „wszystko jest capability”
Każdy wykonawca (LLM, silnik szachowy, parser, SAT solver, test runner) jest opakowany jako **capability** o zunifikowanym interfejsie wywołania.

- Wnętrze wykonawcy nie jest istotne dla warstwy wyższej.
- Istotne są: typ wejścia/wyjścia, kontrakty, koszty, polityki.

**Konsekwencja:** LLM i non-LLM są wymienialne w tym samym mechanizmie delegacji.

### 1.2. Jedna konwencja wywołania: `invoke(cap, ctx, req) -> result`
- `cap` – uchwyt do capability (nie „nazwa modelu”, tylko wartość/capability-id + meta).
- `ctx` – kontekst wykonania (trace, budżet, call stack, cache policy, dostępność).
- `req` – request zawsze **strukturalny**, nie „goły prompt”.

**Konsekwencja:** system skaluje się na dowolną głębokość wywołań i heterogenicznych wykonawców.

### 1.3. Stratyfikacja przez IR: program jako dane (AST/Plan)
Zamiast „tekstu z dziurami” używamy IR (np. EDN), gdzie odpowiedź/rozwiązanie jest reprezentowane jako **AST** zawierające węzły wywołań (`CallNode`).

**Konsekwencja:** delegacja i składanie wyników jest deterministyczne, walidowalne i debugowalne.

### 1.4. Wynik ma jeden z kształtów: `Value | Plan | Stream`
Każde capability może zwrócić:
- `Value` – gotową wartość w schemacie wyjścia,
- `Plan` – kolejny plan do wykonania (rekurencja; A→B→C bez specjalnych case’ów),
- `Stream` – opcjonalnie: inkrementalne wyniki (dla równoległości/latencji).

**Konsekwencja:** nie potrzebujesz osobnego mechanizmu dla „agentów”; capability może być kompilatorem (zwraca Plan) lub solverem (zwraca Value).

### 1.5. Aplikator (runtime) jest ewaluatorem (eval/apply)
Runtime wykonuje IR:
- wykrywa `CallNode`,
- rozwiązuje wykonawcę (late binding),
- wykonuje,
- waliduje wynik,
- podstawia do środowiska,
- kontynuuje eval (rekurencyjnie).

**Konsekwencja:** jeden mechanizm działa dla:
- interleaved/strict (wykonuj call natychmiast),
- plan-first/lazy (zbieraj i wykonuj później),
- hybrydy (prefetch/race + reszta strict).

### 1.6. Delegacja jest deklaratywna (intent + requires), a nie „adresowa”
W planie zapisujesz:
- `:intent` (co ma być zrobione),
- `:requires` (kontrakty i schemat wyjścia),
- `:dispatch` (kandydaci i polityki),
a nie „na sztywno: użyj modelu B”.

**Konsekwencja:** aplikator może zmienić wykonawcę, gdy:
- capability niedostępne,
- budżet/latencja ograniczona,
- wynik nie przechodzi bramki jakości.

---

## 2) Wnioski: co daje największą użyteczność

### 2.1. Uniformizacja problemu „wkomponowania w rozumowanie”
Nie próbujemy wstrzykiwać stanów wewnętrznych między modelami.
Uniformizacja zachodzi przez:
- **schemat wyjścia** (`:out/schema`)
- **kontrakty** (`:quality/must`, walidatory, testy)
- opcjonalnie **judge rubric** (LLM-judge)

**Efekt praktyczny:** dowolny wykonawca może być podmieniony, jeśli dostarcza wynik w tym samym schemacie i przechodzi te same bramki.

### 2.2. Funkcje wyższego rzędu (HOF) w tym systemie
- Capability jest wartością (jak funkcja).
- „Przekazywanie funkcji” = przekazywanie capability-handle + schematów + kontraktu.
- Model/agent na poziomie wyższym może **produkować plan** (funkcję w postaci AST) zawierający wywołania innych capabilities.

**Efekt praktyczny:** A może pełnić rolę „kompilatora polityki”, B „solvera”, C „naprawiacza”, a runtime to „eval”.

### 2.3. Lazy aplikator jako warstwa odpornościowa (resilience)
Late binding umożliwia:
- fallback po dostępności,
- fallback po kosztach,
- fallback po jakości (ponowienie / zmiana wykonawcy / repair loop),
- race (spekulacyjne wykonanie kilku i wybór najlepszego).

**Efekt praktyczny:** system nie jest kruchy wobec niedostępności jednego modelu/usługi.

### 2.4. Quality-aware dispatch (routing też po jakości)
Aplikator nie tylko „wybiera działającego”, ale:
- odpala kandydatów (kaskadowo lub turniejowo),
- weryfikuje przez `must` + `score`,
- dopiero potem commituje wynik.

**Efekt praktyczny:** jakość staje się częścią semantyki wywołania, a nie przypadkowym efektem wykonawcy.

---

## 3) Minimalne schematy (EDN) – kanoniczna konwencja

### 3.1. Registry capability (meta)
```edn
{:cap/id :llm/code
 :cap/kind :llm
 :io/in-schema  :req/code
 :io/out-schema :res/patch+tests
 :cap/version "2026-02-15"          ;; opcjonalne, ale zalecane
 :cap/cost {:latency-ms 1200}       ;; opcjonalne
 :cap/limits {:timeout-ms 15000}    ;; opcjonalne
 :cap/tags #{:coding :clojure}}     ;; opcjonalne
```

### 3.2. Request (zunifikowany)
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :task  {:intent :code/patch
         :requires {:out-schema :res/patch+tests}
         :dispatch {:candidates [:llm/code :llm/meta]
                    :policy :quality-aware}}
 :input {:task "Fix NPE in foo()"
         :lang :clojure}
 :context {:summary "krótki kontekst"}
 :constraints {:no-web true :language :pl}
 :done {:must #{:schema-valid :patch-applies}
        :should #{:tests-pass}
        :score-min 0.8}
 :budget {:max-tokens 1200 :max-roundtrips 3}
 :effects {:allowed #{:none}}}
```

### 3.3. Response (zunifikowany)
Sukces:
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :result {:type :value
          :out {:patch "...diff..." :tests ["..."]}
          :usage {:mode :live}}}
```

Plan:
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :result {:type :plan
          :plan {:id "p-01" :nodes [...]}
          :bindings {:summary "NPE w foo()"}}}
```

Błąd:
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :error {:type :schema/invalid
         :retryable? true
         :where :model-output
         :details {:path [:result :out]}}}
```

### 3.4. Plan/AST z CallNode (rekurencja A→B→C)
```edn
{:id "p-01"
 :nodes
 [{:op :call
   :id "c-01"
   :intent :context/summarize
   :input {:text "..."}
   :output {:schema :res/context-summary}
   :dispatch {:candidates [:llm/meta]}
   :as :summary}

  {:op :call
   :id "c-02"
   :intent :text/respond
   :input {:prompt {:slot/id :summary}}
   :output {:schema :res/text}
   :dispatch {:candidates [:llm/voice]}
   :as :answer}

  {:op :emit
   :input {:final :answer}}]}
```

`{:slot/id ...}` to miejsce wstrzyknięcia wartości z `:bindings` albo z kontekstu aplikatora.

### 3.5. CallNode dla non-LLM (ten sam IR)
```edn
{:op :call
 :id "c-chess-01"
 :intent :chess/best-move
 :input {:fen "..." :want {:pv 5 :eval true}}
 :output {:schema :res/chess.move+pv+eval}
 :dispatch {:candidates [:chess/engine-v7]
            :strategy :cascade}
 :done {:must #{:legal-move}}
 :as :move1}
```

## 4) Semantyka wykonania (warianty)

### 4.1. Interleaved/strict
- runtime wykonuje `CallNode` natychmiast,
- wynik trafia do env,
- kolejne kroki zależą od poprzednich.

### 4.2. Plan-first/lazy
- wykonawca produkuje większy plan,
- runtime wykonuje batch (równoległość, late binding).

### 4.3. Hybryda (zalecane)
- strict dla zależności,
- lazy dla fragmentów niezależnych,
- race/prefetch dla krytycznych ścieżek.

## 5) MVP i status implementacji

1. Capability registry (ID -> meta, schematy): częściowo wdrożone.
2. IR/Plan + CallNode: wdrożone minimalnie (`:plan`, materializacja slotów, `CallNode`).
3. Aplikator/evaluator (resolve/execute/rekurencja): wdrożone minimalnie (`:let`, `:call`, `:emit`, plan->plan).
   Wdrożone: verify (`:done`), retry/fallback (`:retry`, `:switch-on`, kandydaci), judge-loop (opcjonalny, konfigurowalny).
4. Walidatory kontraktów: wdrożone (`request` i `response` envelope).
5. Judge capability: wdrożone opcjonalnie (gałąź `:quality/judge`, domyślnie wyłączone w `prod`, możliwe włączenie np. w `dev`).

## 6) Dlaczego to jest stratified

- Wyższe warstwy operują na `:intent` i kontraktach, nie na nazwach modeli.
- Niższa warstwa (aplikator) wiąże wywołania do konkretnych wykonawców w runtime.
- “Funkcje jako wartości” realizują się przez capability-handle i plany jako dane.

To daje kompozycyjność, odporność (late binding) i kontrolę jakości w jednym mechanizmie.
