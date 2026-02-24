# Ferment - Design

## 1. System Goal

Ferment is an AI agent engine developed iteratively through budding:
- first, a small working core,
- then additional "buds" added as independent building blocks,
- without rewriting the whole system.

Main goal:
- keep a simple data contract,
- keep a deterministic orchestration pipeline,
- separate layers: domain -> orchestration -> adapters -> runtime.

## 2. Operational Sources of Truth

This document and `doc/stratification.md` are the joint operational source of truth:
- `doc/design.md` defines architecture, modules, and development plan,
- `doc/stratification.md` defines execution semantics (capabilities, IR, quality applicator).

Consistency rule:
- call/delegation semantics follow `doc/stratification.md`,
- module boundaries and development stages follow `doc/design.md`.

## 3. Architectural Principles

1. Data before objects: flow is based on EDN maps and explicit keys.
2. Thin interfaces: module boundary functions accept and return maps.
3. Side-effect separation: I/O only in adapters.
4. Determinism where possible: same input => same execution plan.
5. Budding: a new feature goes into a new module and is wired via registry/router.
6. Everything is a capability: LLM and non-LLM are invoked through one contract.
7. Delegation is late-bound and quality-aware: executor choice can change at applicator time.

## 4. Call Semantics (Stratification Integration)

### 4.1. One Call Convention
`invoke(cap, ctx, req) -> result`

- `cap`: capability handle (`:cap/id` + metadata), not a hardcoded model name,
- `ctx`: execution context (trace, budget, call stack, policies),
- `req`: structured request, not a raw prompt.

### 4.2. Program as Data (IR)
Execution plan is represented as IR/AST (EDN) with call nodes.

Minimal capability result contract:
- `Value` - final value,
- `Plan` - next plan to execute (recursive delegation),
- `Stream` - optional incremental output.

### 4.3. Quality Applicator
Runtime acts as an evaluator:
- recognizes call nodes,
- resolves executor (late binding),
- executes, validates, and commits result,
- performs fallback/race/repair-loop according to quality policies.

### 4.4. Declarative Delegation
Plans declare:
- `:intent`,
- `:requires` (contracts and output schema),
- `:dispatch` (candidates and policies),
not "use model X directly".

## 5. System Layers

### 5.1. Domain Layer (pure)
- intent classification,
- response plan construction,
- routing to role/capability,
- prompt composition.

Requirement: no I/O and no provider-specific dependencies.

### 5.2. Orchestration Layer (workflow)
- step sequencing (pipeline),
- retry/timeout/fallback policies,
- budget and quality control.

Requirement: control logic without direct `curl`/process calls.

### 5.3. Adapter Layer (integration)
- LLM adapters (currently mostly MLX server via HTTP; optional command/stdin fallback),
- non-LLM adapters (system tools, solvers),
- config/environment adapter.

Requirement: all side effects and protocol mapping live here.

### 5.4. Runtime Layer (entrypoints)
- CLI (`-main`),
- batch/server mode,
- config bootstrap (`ferment.system/read-configs`).

Requirement: thin composition layer for all others.

## 6. Module Map (current and target)

Current building blocks:
- `ferment.core` - contract-driven capability calls (`invoke-with-contract`, `call-capability`), canonical domain entrypoints (`classify-intent`, `build-plan`), meta/solver/voice routing, and plan execution,
- `ferment.workflow` - IR evaluator (`:let`, `:call`, `:emit`, recursive `:plan`) with retry/fallback/checks,
- `ferment.router`, `ferment.resolver`, `ferment.contracts`, `ferment.protocol` - routing, capability registry, and request/response validation,
- `ferment.model` - model runtime (processes, HTTP invoke, session workers, freeze/thaw/TTL),
- `ferment.http` - HTTP bridge (`/v1/act`, `/v1/session`, `/v1/admin`, `/health`, `/routes`, `/diag/telemetry`),
- `ferment.session`, `ferment.session.store` - sessions and working memory (session vars, policies, TTL/freeze classes, DB backend),
- `ferment.system`, `ferment.env`, `ferment.env.file`, `ferment.readers` - config and service lifecycle,
- `ferment.db`, `ferment.auth*`, `ferment.roles`, `ferment.effects`, `ferment.oplog*` - DB/Auth/RBAC/effects/audit layers.

Target buds:
- `ferment.contracts` + `ferment.protocol` - capability contracts, envelope, quality policies,
- `ferment.router` - intent and quality policy routing,
- `ferment.workflow` - step-plan execution (IR eval/apply),
- `ferment.adapters.*` - model providers and tools,
- `ferment.memory` - session state, context, cache,
- `ferment.telemetry` - structured logs, metrics, trace-id.
  - current `/diag/telemetry` snapshot includes `:act`, `:workflow`, and canonical KPI in `:kpi` (`parse-rate`, `retry-rate`, `fallback-rate`, `judge-pass-rate`, `failure-taxonomy`).
- protocol prompt packages (`resources/config/common/prod/protocol.edn`) are stratified:
  - `:prompts/:default` (global core),
  - `:prompts/:roles` (router/solver/voice/coder/judge),
  - `:prompts/:intents` (intent semantic refinements).
  Runtime composes prompts from these layers; `:intents/*/:system` is the canonical full override.

### 6.1. Implementation Status (2026-02-22)

Integrant config branches are already split and active:
- `:ferment.runtime/default` - orchestrator runtime input (refs to `:router`, `:resolver`, `:protocol`, `:roles`, `:session`, `:oplog`, `:models`, plus effect scope),
- `:ferment.core/default` - core service (`:call-capability`, `:classify-intent`, `:build-plan`, plus `:solver!`/`:voice!`) initialized from runtime,
- `:ferment.model.defaults/*`, `:ferment.model.id/*`, `:ferment.model.runtime/*`, `:ferment.model/*`, `:ferment/models` - complete model selection/runtime config.
- `:ferment.session.store/default :session-vars/contract` supports:
  - `:policy/*` (read/write/delete by intent and operation),
  - `:class/by-namespace` + `:class/policy` (TTL/freeze by data class),
  - `:request/default-bindings` (automatic session var injection into `/v1/act` request).

Current bootstrap:
- production: `ferment.app/start!` (loads `resources/config/common/prod` + `resources/config/local/prod`),
- developer: `ferment.app/start-dev!` (loads `prod` then overlays `dev`; `dev` overrides branches),
- admin: `ferment.app/start-admin!` (loads `prod` then overlays `admin`),
- test: `ferment.app/start-test!` (loads `prod` then overlays `test`),
- smoke-live: `ferment.app/start-test-live!` (loads `prod` then overlays `test-live`).

Profile convention:
- application profiles: `:prod`, `:dev`, `:admin`, `:test`, `:test-live`,
- `FERMENT_PROFILE` is auxiliary (for shell scripts), while runtime profile remains available at `:ferment.app/properties`.

Practical objective:
- keep a clear layered configuration model (`common` + `local`, `prod` + environment overlay), with deterministic file loading (non-recursive scan + source-order deduplication) and no divergence between app and shell scripts.

## 7. Data Contracts (v1)

### 7.1. Input
```clojure
{:proto 1
 :trace {:id "demo-1"}
 :task  {:intent :text/respond
         ;; optional:
         ;; :cap/id :llm/voice
         ;; :requires {:type :value :out-schema :res/text}
         }
 :input {:prompt "Explain ACID in two sentences."}
 :context {}
 :constraints {:language :en}
 :done {:must #{:schema-valid}}
 :budget {:max-roundtrips 3}
 :effects {:allowed #{:none}}}
```

### 7.2. Orchestration Plan
```clojure
{:id "plan-1"
 :nodes [{:op :call
          :as :solver
          :intent :problem/solve
          :input {:prompt "Explain ACID and provide an example."}
          :dispatch {:candidates [:llm/solver]}}
         {:op :call
          :as :voice
          :intent :text/respond
          :input {:prompt {:slot/path [:solver :text]}}
          :dispatch {:candidates [:llm/voice]}}
         {:op :emit
          :input {:text {:slot/path [:voice :text]}}}]}
```

### 7.3. Response
```clojure
;; success
{:proto 1
 :trace {:id "demo-1"}
 :result {:type :value
          :out {:text "ACID is..."}
          :usage {:mode :live}}}

;; error
{:proto 1
 :trace {:id "demo-1"}
 :error {:type :route/decide-failed
         :message "LLM invocation failed after retries"
         :retryable? true}}
```

## 8. Core Functions (MVP)

1. `classify-intent` - recognizes request intent.
2. `build-plan` - builds a step plan based on intent.
3. `resolve-capability` - chooses candidate using policy.
4. `execute-plan` - executes plan steps through applicator.
5. `call-capability` - unified capability invocation point.
6. `respond` - returns final response + quality metadata.

## 9. Budding Plan (stages)

### Stage A - Stable core
- extract pure pipeline into `ferment.workflow`,
- unify data shape (`request`, `plan`, `response`),
- add contract tests for the pipeline.

### Stage B - Router and roles
- move heuristics into `ferment.router`,
- add role profiles (solver/voice),
- separate prompt-building from model adapter.

### Stage C - Capability runtime
- introduce capability registry and `invoke(cap, ctx, req)`,
- support `Value | Plan | Stream`,
- add fallback/race/retry and quality gates.

### Stage D - Memory and observability
- `session-state` and `request/id` correlation,
- explicit `session vars` working-memory contract (key namespaces, TTL, freeze/thaw, limits),
- telemetry events (start/stop/error),
- local response cache (optional).

## 10. Consistency with `doc/stratification.md` (no conflicts)

No semantic conflicts detected; documents are complementary.

Capabilities that remain preserved:
1. heterogeneous execution (LLM + non-LLM) through capability,
2. arbitrary delegation depth (A->B->C->...),
3. late runtime binding of executor,
4. quality-aware dispatch (not just availability/latency),
5. plan representation as IR/AST and evaluator execution.

## 11. 1:1 Compatibility Map (`stratification` -> `design`)

| `doc/stratification.md` | `doc/design.md` | Status |
|---|---|---|
| 1.1 "Everything is a capability" | 3.6, 4.1, 6, 8.5 | Covered |
| 1.2 `invoke(cap, ctx, req) -> result` | 4.1, 8.5, 9.C | Covered |
| 1.3 IR as AST/Plan | 4.2, 6 (`ferment.workflow`), 9.C | Covered |
| 1.4 `Value | Plan | Stream` | 4.2, 9.C | Covered |
| 1.5 Applicator as evaluator | 4.3, 8.4, 9.C | Covered |
| 1.6 Declarative delegation (`intent` + `requires` + `dispatch`) | 4.4, 7.2 | Covered |
| 2.1 Uniformization through schema and quality contracts | 7.2, 7.3, 10 | Covered |
| 2.2 HOF/capability as value | 3.6, 4.2, 8.3 | Covered |
| 2.3 Late binding/lazy runtime as resilience | 3.7, 4.3, 9.C | Covered |
| 2.4 Quality-aware dispatch | 3.7, 4.3, 7.2, 10 | Covered |
| 3. Minimal EDN schemas (registry/dispatch/result) | 7.2, 7.3, 9.C | Covered |

Integration notes:
1. No semantic conflicts; both documents are complementary.
2. Stratification capabilities are preserved: heterogeneous execution, recursive delegation, late binding, and quality-aware dispatch remain architecture requirements.
3. Remaining operational tuning mainly concerns per-intent quality parameters (prompts, checks, thresholds, judge), not contract shape.

## 12. Definition of "done" for new buds

A new bud is "done" when:
1. it has explicit input/output contract,
2. it has a unit test for that contract,
3. it does not expand responsibilities of existing modules,
4. it can be enabled/disabled via config,
5. it does not break rules from `doc/stratification.md`.

---

Working document v0.4. Each iteration updates:
- data contracts,
- module map,
- stage plan,
- quality invariants and dispatch rules.
