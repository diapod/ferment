# Domenowy Backlog

Stan: po wdrożeniu kontraktów, sesji, RBAC/effects i flow `/v1/act`.

## Priorytety

1. [x] Per-intent quality tuning (operacyjnie)
   - Dostrajać `:done`, `:checks`, `judge` i prompty per-intent w `resources/config/common/prod/protocol.edn`.
   - Cel: mniej retry/fallback i stabilniejsze wyniki `meta -> solver -> voice`.
   - Wdrożone:
     - prompty systemowe dla `:route/decide`, `:context/summarize`, `:problem/solve`, `:code/*`, `:eval/grade`,
     - doprecyzowane limity `:constraints/:max-chars` per intent,
     - korekta polityk jakości dla intentów kodowych (`:checks` i `:done`) oraz defaultowego `:done/:should`.

2. [x] Polityka routingu jako decyzja produktowa
   - Ustalić domyślne `routing.meta?`, `routing.strict?`, `routing.force?` dla `dev/test-live/prod`.
   - Określić, kiedy fail-open vs fail-closed.
   - Cel: przewidywalne zachowanie `/v1/act`.
   - Wdrożone:
     - `resources/config/common/prod/router.edn`: `:defaults {:meta? true :strict? false :force? false :on-error :fail-open}`,
     - `resources/config/common/dev/router.edn`: `:defaults {:meta? true :strict? true :force? false :on-error :fail-closed}`,
     - `resources/config/common/test-live/router.edn`: `:defaults {:meta? true :strict? false :force? false :on-error :fail-open}`,
     - runtime HTTP korzysta z efektywnej konfiguracji (merge `router/defaults` + request `:routing`),
     - semantyka błędu routingu: `:on-error :fail-closed` => 502, `:on-error :fail-open` => fallback do statycznego routingu,
     - request może nadpisać `:on-error` i wymusić tryb inny niż default profilu.

3. [x] Kontrakt pamięci roboczej sesji
   - Uporządkować namespace'y session-vars (co wolno zapisywać, co automatycznie trafia do kontekstu).
   - Doprecyzować TTL i zasady freeze/thaw dla klas danych.
   - Cel: spójna pamięć kontekstowa bez przecieków semantycznych.
   - Wdrożone:
     - kontrakt `session-vars` rozszerzony o `:class/default`, `:class/by-namespace`, `:class/policy` (TTL i freeze policy per klasa danych),
     - `put/del` przy zamrożonej sesji sprawdzają uprawnienia per klucz/klasę (z fallbackiem do globalnych `:freeze/*`),
     - TTL zmiennej jest wyliczane per klasa danych (`:ttl/default-ms` / `:ttl/max-ms`) z możliwością nadpisania `:ttl-ms` w opcji zapisu,
     - kontrakt wspiera `:request/default-bindings`, które deklaratywnie określają jakie `session-vars` automatycznie wstrzykiwać do requestu (`:constraints`, `:input`, `:context`),
     - `/v1/act` czyta bindingi z kontraktu store zamiast z hardcodowanej listy kluczy.

4. [x] Polityka ról i efektów na planach narzędziowych
   - Dopracować macierz `role -> operation/effect` (szczególnie `:process/run`, `:net/http`, `:fs/write`).
   - Dopiąć scenariusze graniczne i jednoznaczne kody błędów.
   - Cel: jeden model autoryzacji end-to-end w całym planie wykonania.
   - Wdrożone:
     - produkcyjna polityka ról przestawiona na `:authorize-default? false` (default-deny dla nieznanych operacji/efektów),
     - ujednolicenie kodu błędu dla braku deklaracji efektów w `:tool` (`:effects/invalid-input` + `:reason :effects/not-declared`),
     - testy graniczne dla `workflow`, `effects`, `http` i `roles` pod kątem autoryzacji efektów i mapowania kodów błędów.

5. [x] Konsolidacja telemetrii jakości
   - Ustalić kanoniczne KPI: parse-rate, retry-rate, fallback-rate, judge pass-rate, failure taxonomy dla `/v1/act`.
   - Cel: sterowanie tuningiem na metrykach.
   - Wdrożone:
     - `/diag/telemetry` zwraca gałąź `:kpi` z metrykami: `:parse-rate`, `:retry-rate`, `:fallback-rate`, `:judge-pass-rate`,
     - `:kpi/:failure-taxonomy` agreguje błędy po typie (`:by-type`) i domenie (`:by-domain`),
     - workflow telemetry rozszerzone o `:quality/judge-pass` i `:quality/judge-fail`,
     - testy regresji KPI/taxonomy dla `http`, `workflow` i endpointu diagnostycznego.

6. [x] Pakiety promptów dla ról
   - Dopracować prompty systemowe `meta`, `solver`, `voice` pod kontrakty (bez `<think>`, mniej driftu formatu).
   - Cel: mniejsza halucynacja i lepsza zgodność z protokołem w trybie live.
   - Wdrożone:
     - `resources/config/common/prod/protocol.edn` ma pakiety promptów pod `:prompts/:default`, `:prompts/:roles`, `:prompts/:intents`,
     - intencje `:route/decide`, `:context/summarize`, `:text/respond`, `:problem/solve` korzystają z pakietów zamiast długich promptów inline,
     - budowanie promptu w runtime wspiera kompozycję `default + role + intent` (z zachowaniem kompatybilnego override przez `:system` i `:system/prompt`),
     - testy regresji pokrywają zarówno kompozycję pakietów, jak i legacy override,
     - tuning operacyjny iteracja 2: zaostrzone prompty i limity znaków dla `route/decide`, `context/summarize`, `text/respond`, `problem/solve` oraz twarde `:no-list-expansion` dla `text/respond`.
