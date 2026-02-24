# Domain Backlog

Status: after delivering contracts, sessions, RBAC/effects, and `/v1/act` flow.

## Priorities

1. [x] Per-intent quality tuning (operational)
   - Tune `:done`, `:checks`, `judge`, and per-intent prompts in `resources/config/common/prod/protocol.edn`.
   - Goal: fewer retries/fallbacks and more stable `meta -> solver -> voice` behavior.
   - Delivered:
     - system prompts for `:route/decide`, `:context/summarize`, `:problem/solve`, `:code/*`, `:eval/grade`,
     - refined per-intent `:constraints/:max-chars` limits,
     - quality policy updates for coding intents (`:checks` and `:done`) and default `:done/:should`.

2. [x] Routing policy as a product decision
   - Define default `routing.meta?`, `routing.strict?`, `routing.force?` for `dev/test-live/prod`.
   - Define when to fail-open vs fail-closed.
   - Goal: predictable `/v1/act` behavior.
   - Delivered:
     - `resources/config/common/prod/router.edn`: `:defaults {:meta? true :strict? false :force? false :on-error :fail-open}`,
     - `resources/config/common/dev/router.edn`: `:defaults {:meta? true :strict? true :force? false :on-error :fail-closed}`,
     - `resources/config/common/test-live/router.edn`: `:defaults {:meta? true :strict? false :force? false :on-error :fail-open}`,
     - HTTP runtime uses effective config (`router/defaults` + request `:routing`),
     - routing error semantics: `:on-error :fail-closed` => 502, `:on-error :fail-open` => fallback to static routing,
     - request-level `:on-error` can override profile defaults.

3. [x] Working-memory session contract
   - Standardize session-vars namespaces (what can be written, what is auto-injected into context).
   - Clarify TTL and freeze/thaw rules by data class.
   - Goal: coherent contextual memory without semantic leakage.
   - Delivered:
     - `session-vars` contract extended with `:class/default`, `:class/by-namespace`, `:class/policy` (TTL and freeze policy by data class),
     - frozen-session `put/del` now checks permissions by key/class (with fallback to global `:freeze/*`),
     - variable TTL is computed per data class (`:ttl/default-ms` / `:ttl/max-ms`) with optional per-write `:ttl-ms` override,
     - contract supports `:request/default-bindings` for declarative auto-injection of `session-vars` into request paths (`:constraints`, `:input`, `:context`),
     - `/v1/act` reads bindings from store contract instead of a hardcoded key list.

4. [x] Role/effect policy for tool plans
   - Refine the `role -> operation/effect` matrix (especially `:process/run`, `:net/http`, `:fs/write`).
   - Close edge-case scenarios and enforce unambiguous error codes.
   - Goal: one authorization model end-to-end for the full execution plan.
   - Delivered:
     - production role policy switched to `:authorize-default? false` (default-deny for unknown operations/effects),
     - unified error code for missing effect declaration in `:tool` (`:effects/invalid-input` + `:reason :effects/not-declared`),
     - edge-case tests added for `workflow`, `effects`, `http`, and `roles` around effect authorization and error mapping.

5. [x] Quality telemetry consolidation
   - Define canonical KPIs: parse-rate, retry-rate, fallback-rate, judge pass-rate, failure taxonomy for `/v1/act`.
   - Goal: drive tuning from metrics.
   - Delivered:
     - `/diag/telemetry` now returns `:kpi` branch with `:parse-rate`, `:retry-rate`, `:fallback-rate`, `:judge-pass-rate`,
     - `:kpi/:failure-taxonomy` aggregates errors by type (`:by-type`) and by domain (`:by-domain`),
     - workflow telemetry extended with `:quality/judge-pass` and `:quality/judge-fail`,
     - KPI/taxonomy regression tests added for `http`, `workflow`, and diagnostics endpoint.

6. [x] Role prompt packages
   - Refine system prompts for `meta`, `solver`, `voice` against contracts (no `<think>`, less format drift).
   - Goal: lower hallucination rate and better protocol conformance in live mode.
   - Delivered:
     - `resources/config/common/prod/protocol.edn` includes prompt packages under `:prompts/:default`, `:prompts/:roles`, `:prompts/:intents`,
     - intents `:route/decide`, `:context/summarize`, `:text/respond`, `:problem/solve` use prompt packages instead of long inline prompts,
     - runtime prompt builder supports composition `default + role + intent` while keeping full override compatibility via `:system` and `:system/prompt`,
     - regression tests cover both package composition and legacy overrides,
     - operational tuning iteration 2: stricter prompts and output limits for `route/decide`, `context/summarize`, `text/respond`, `problem/solve`, plus hard `:no-list-expansion` for `text/respond`.

7. [x] Materialize target module buds (`adapters`, `memory`, `telemetry`)
   - Split integration concerns into explicit namespaces:
     - `ferment.adapters.model` (provider-specific model invoke/start/stop),
     - `ferment.adapters.tool` (tool-side effect adapters),
     - `ferment.memory` (session/context/cache facade),
     - `ferment.telemetry` (counters/events API and sinks).
   - Goal: align runtime code layout with design stratification and reduce cross-layer coupling.
   - Delivered:
     - added namespace facades: `src/ferment/adapters/model.clj`, `src/ferment/adapters/tool.clj`, `src/ferment/memory.clj`, `src/ferment/telemetry.clj`,
     - rewired `core` to use `tool` adapter for plan tools and `memory` facade for runtime session operations,
     - rewired `http` to use `model` adapter and `memory` facade in session/runtime endpoints and defaults injection,
     - rewired `workflow` to use shared telemetry helpers from `ferment.telemetry`,
     - regression check: `bin/test-full` green after refactor.

8. [x] Normalize core domain API names (`classify-intent`, `build-plan`, `call-capability`)
   - Define canonical public entrypoints and map current internals to them (`resolve-capability`, `execute-plan`, `respond!`).
   - Goal: make domain pipeline explicit and discoverable as stable API.
   - Delivered:
     - added canonical public entrypoints in `ferment.core`: `classify-intent`, `build-plan`, `call-capability`,
     - replaced internal/runtime usage of legacy names with canonical API in `ferment.http` and tests,
     - renamed workflow capability resolver API to `resolve-capability`,
     - updated core service map to expose `:classify-intent`, `:build-plan`, `:call-capability`,
     - updated design docs to use canonical naming and removed legacy prompt override naming.

9. [ ] Complete Stage D observability (`start/stop/error` events + optional response cache)
   - Add lifecycle telemetry events for app/runtime/http/model/session start-stop-failure transitions.
   - Add local response cache behind config switch (intent-aware key, TTL, bounded size, safe invalidation).
   - Goal: operational visibility and predictable latency/cost reduction on repeat requests.
   - Done when:
     - lifecycle event schema is emitted and visible via diagnostics/oplog,
     - response cache is disabled by default, can be enabled per profile, and has regression tests,
     - cache hit/miss is included in telemetry snapshot.
