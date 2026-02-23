# Stratification of Calls

Stratification of calls to LLM and non-LLM solvers is realized via capabilities, IR and quality applicator.

The assumptions and conclusions below describe a stratified design architecture for a multi-agent system where:

- execution units can be LLMs or non-LLM solvers,
- calls can nest to arbitrary depth (A->B->C->...),
- delegation is *late-bound* (lazy applicator may switch executor when address/capability is unavailable),
- routing is **quality-aware** (fallback based on quality, not only availability/latency).

## 1) Key assumptions

### 1.1. Abstraction: "everything is a capability"
Each executor (LLM, chess engine, parser, SAT solver, test runner) is wrapped as a **capability** with a unified invocation interface.

- Internal implementation is irrelevant to higher layers.
- Relevant parts are input/output type, contracts, cost, and policies.

**Consequence:** LLM and non-LLM executors are interchangeable within one delegation mechanism.

### 1.2. One invocation convention: `invoke(cap, ctx, req) -> result`
- `cap` - capability handle (not a model name; a value/capability-id + metadata).
- `ctx` - execution context (trace, budget, call stack, cache policy, availability).
- `req` - always a **structured** request, not a raw prompt.

**Consequence:** system scales to arbitrary call depth and heterogeneous executors.

### 1.3. Stratification by IR: program as data (AST/Plan)
Instead of "text with holes", use IR (for example EDN), where answer/solution is represented as an **AST** containing call nodes (`CallNode`).

**Consequence:** delegation and composition are deterministic, validatable, and debuggable.

### 1.4. Result has one of the forms: `Value | Plan | Stream`
Every capability can return:
- `Value` - final value in output schema,
- `Plan` - next plan to execute (recursion; A->B->C without special cases),
- `Stream` - optional incremental results (for latency/concurrency).

**Consequence:** no separate mechanism is required for "agents"; a capability can be a compiler (returns Plan) or solver (returns Value).

### 1.5. Applicator (runtime) is evaluator (eval/apply)
Runtime executes IR:
- detects `CallNode`,
- resolves executor (late binding),
- executes,
- validates result,
- injects into environment,
- continues eval (recursively).

**Consequence:** one mechanism supports:
- interleaved/strict (execute call immediately),
- plan-first/lazy (collect and execute later),
- hybrid (prefetch/race + strict remainder).

### 1.6. Delegation is declarative (`intent + requires`), not address-based
Plan should include:
- `:intent` (what to do),
- `:requires` (contracts and output schema),
- `:dispatch` (candidates and policies),
not "hardcode model B".

**Consequence:** applicator may switch executor when:
- capability is unavailable,
- budget/latency is constrained,
- result fails quality gate.

---

## 2) Conclusions: what gives the highest utility

### 2.1. Uniformizing "composition into reasoning"
Do not attempt to inject internal state between models.
Uniformization should happen through:
- **output schema** (`:out/schema`)
- **contracts** (`:quality/must`, validators, tests)
- optional **judge rubric** (LLM judge)

**Practical effect:** any executor can be replaced if it returns the same schema and passes the same gates.

### 2.2. Higher-order functions (HOF) in this system
- Capability is a value (like a function).
- "Passing functions" means passing capability handle + schemas + contract.
- A higher-level model/agent can **produce a plan** (function as AST data) with calls to other capabilities.

**Practical effect:** A can be a policy compiler, B a solver, C a repairer, while runtime is eval.

### 2.3. Lazy applicator as resilience layer
Late binding enables:
- availability fallback,
- cost fallback,
- quality fallback (retry / executor switch / repair loop),
- race (speculative multi-run + choose best).

**Practical effect:** system is not brittle when one model/service is unavailable.

### 2.4. Quality-aware dispatch (routing by quality too)
Applicator does not only "pick one that works", it:
- executes candidates (cascade or tournament),
- verifies using `must` + `score`,
- commits only after validation.

**Practical effect:** quality becomes part of call semantics, not an accidental executor side effect.

---

## 3) Minimal schemas (EDN) - canonical convention

### 3.1. Capability registry (metadata)
```edn
{:cap/id :llm/code
 :cap/kind :llm
 :io/in-schema  :req/code
 :io/out-schema :res/patch+tests
 :cap/version "2026-02-15"          ;; optional but recommended
 :cap/cost {:latency-ms 1200}       ;; optional
 :cap/limits {:timeout-ms 15000}    ;; optional
 :cap/tags #{:coding :clojure}}     ;; optional
```

### 3.2. Request (unified)
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :task  {:intent :code/patch
         :requires {:out-schema :res/patch+tests}
         :dispatch {:candidates [:llm/code :llm/meta]
                    :policy :quality-aware}}
 :input {:task "Fix NPE in foo()"
         :lang :clojure}
 :context {:summary "short context"}
 :constraints {:no-web true :language :en}
 :done {:must #{:schema-valid :patch-applies}
        :should #{:tests-pass}
        :score-min 0.8}
 :budget {:max-tokens 1200 :max-roundtrips 3}
 :effects {:allowed #{:none}}}
```

### 3.3. Response (unified)
Success:
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
          :bindings {:summary "NPE in foo()"}}}
```

Error:
```edn
{:proto 1
 :trace {:id "r-4a2b" :turn 17}
 :error {:type :schema/invalid
         :retryable? true
         :where :model-output
         :details {:path [:result :out]}}}
```

### 3.4. Plan/AST with CallNode (recursive A->B->C)
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

`{:slot/id ...}` is an injection point for values from `:bindings` or applicator context.

### 3.5. CallNode for non-LLM (same IR)
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

## 4) Execution semantics (variants)

### 4.1. Interleaved/strict
- runtime executes `CallNode` immediately,
- result is put into env,
- next steps depend on previous ones.

### 4.2. Plan-first/lazy
- executor produces a larger plan,
- runtime executes batch (parallelism, late binding).

### 4.3. Hybrid (recommended)
- strict for dependencies,
- lazy for independent fragments,
- race/prefetch for critical paths.

## 5) MVP and implementation status

1. Capability registry (ID -> metadata, schemas): delivered (flattened `:ferment.caps.registry/*` + metadata `:cap/version`, `:cap/cost`, `:cap/limits`, `:cap/tags`).
2. IR/Plan + CallNode: delivered (`:plan`, slot materialization, `CallNode`, `:requires` as hard contract filter for capability selection).
3. Applicator/evaluator (resolve/execute/recursion): delivered (`:let`, `:call`, `:emit`, plan->plan, retry/fallback/switch-on, telemetry, effect execution via runtime scope + RBAC).
4. Contract validators: delivered (`request` and `response` envelope + per-intent `:in-schema`/`:out-schema` validation + routing plan validation).
5. Judge capability: delivered as optional; global default is disabled, while `prod` runs canary per intent (`:text/respond`, `:code/patch`, `:max-attempts 1`).

## 6) Why this is stratified

- Higher layers operate on `:intent` and contracts, not model names.
- Lower layer (applicator) binds calls to concrete executors at runtime.
- "Functions as values" are realized via capability handles and plans-as-data.

This gives composability, resilience (late binding), and quality control in one mechanism.
