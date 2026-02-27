# Production Backlog (Tuning Phase)

Status: post-foundation phase complete (`doc/backlogs-done.md`).
Current focus: orchestration quality, natural multi-model behavior, robust contextual memory.

## Priorities

1. [ ] Routing intelligence hardening (`meta -> solver -> voice`)
   - Calibrate routing prompts and constraints for clearer `route/decide` outputs under strict mode.
   - Add deterministic fallback ladder when `route/decide` quality fails.
   - Goal: stable multi-model participation without accidental single-model collapse.
   - Execution package (config):
     - [x] `resources/config/common/prod/protocol.edn`
       - tighten `:prompts/:intents/:route/decide` to enforce one canonical outcome shape (decision or `solver->voice->emit` plan),
       - tighten `:intents/:route/decide/:constraints` (reduce max output size to reduce drift),
       - define explicit retry/switch/fallback policy under `:policy/intents/:route/decide` (no implicit defaults).
     - [x] `resources/config/common/prod/router.edn` and `resources/config/common/prod/capabilities.edn`
       - keep router branch canonical (`#ref`), and add explicit routing-level retry/switch defaults in routing source branch,
       - keep profile-independent fail mode semantics explicit (`:on-error` remains product default, request may override).
     - [x] `resources/config/common/dev/protocol.edn` and `resources/config/common/test-live/protocol.edn`
       - add overlay knobs for strict tuning runs (judge/retry profile) without mutating prod defaults.
   - Execution package (tests):
     - [x] `test/ferment/http_test.clj`
       - add test for deterministic decider retry ladder (`same-cap` retries, then fail mode),
       - add test that fail-open fallback path is explicit in error/details and routing telemetry counters,
       - add test for strict fail-closed with rich, stable diagnostics payload (`failure/type`, `switch-on`, `retry-policy`, rejected candidates).
     - [x] `test/ferment/workflow_test.clj`
       - add candidate-order determinism test (base + policy fallback + routing fallback => stable deduplicated order),
       - add test that route failure classes map deterministically to recoverable/non-recoverable decisions.
     - [x] `test/ferment/contracts_test.clj`
       - extend `:route/decide` contract tests for malformed/empty decision payload variants that currently trigger ambiguous retries.
     - [x] `test/ferment/router_test.clj`
       - validate new router-level tuning keys and invalid combinations fail fast with actionable `ex-data`.
   - Validation run:
     - [x] `bin/test` green,
     - [ ] strict smoke (`/v1/act` with `routing.meta?=true`, `routing.strict?=true`, `routing.force?=true`) shows either canonical multi-model path or explicit fail-closed diagnostics (no silent fallback).
   - Done when:
     - strict mode keeps `route/fail-closed` intentional and explainable,
     - `models/used` shows expected participants for representative prompts,
     - routing failure reasons are explicit in telemetry and response details.

2. [ ] Capability recognition precision (`intent -> cap/id`)
   - Improve capability selection rules for ambiguous natural-language requests.
   - Add negative tests for near-miss intents (avoid wrong cap selection).
   - Goal: high precision in capability dispatch without overfitting.
   - Done when:
     - resolver chooses expected `cap/id` across golden intent/cap matrix,
     - misroutes are observable in telemetry taxonomy,
     - no legacy alias paths are required for canonical resolution.

3. [ ] Reasoning quality control loop (operational)
   - Tighten per-intent `:done/:checks/judge` profiles for `:route/decide`, `:problem/solve`, `:text/respond`.
   - Separate quality thresholds for structure conformance vs semantic adequacy.
   - Goal: fewer empty/degenerate outputs and fewer low-value retries.
   - Done when:
     - retry-rate and fallback-rate drop on smoke scenarios,
     - `eval/must-failed` is actionable (clear must-rule failures),
     - no `<think>`/tool-leak artifacts in user-facing text.

4. [ ] Tool-call protocol discipline (no leakage to user output)
   - Keep internal tool-calling markers as orchestration internals only.
   - Ensure model outputs are normalized before final emission.
   - Goal: natural conversational responses with strict protocol boundaries.
   - Done when:
     - tool markers never leak in `/v1/act` final output,
     - telemetry still captures tool-plan internals for diagnostics,
     - regression probes for leakage are green.

5. [ ] Context memory shaping (session vars as working memory)
   - Define default memory write/read patterns per intent class.
   - Add summarization/compaction policy for long sessions (`freeze/thaw` friendly).
   - Goal: consistent contextual continuity with bounded memory growth.
   - Done when:
     - context recall works across multi-turn calls without semantic drift,
     - memory budget limits are enforced with deterministic eviction/compaction,
     - session-vars behavior is covered by integration tests.

6. [ ] Cross-model context handoff contract
   - Standardize payload handoff from `solver` to `voice` (and future roles) with explicit shape.
   - Preserve key reasoning artifacts without leaking chain-of-thought.
   - Goal: voice layer naturally verbalizes solver outcome instead of echoing raw draft artifacts.
   - Done when:
     - solver output consumed by voice is structured and schema-validated,
     - voice response quality improves on “explain + example” tasks,
     - no prompt-format bleed between roles.

7. [ ] Adaptive retry/fallback policy tuning per intent
   - Tune `same-cap-max`, `fallback-max`, and `switch-on` sets per intent/cap profile.
   - Differentiate policy for low-latency chat vs high-quality solve paths.
   - Goal: better latency/quality tradeoff by intent.
   - Done when:
     - retry/fallback policy is declarative and intent-specific in config,
     - telemetry KPIs show improved success-per-attempt ratio,
     - no uncontrolled retry bursts in live profile.

8. [ ] Production telemetry views for orchestration tuning
   - Add focused views/queries for routing path quality and memory effectiveness.
   - Expose interpretable KPIs: participant diversity, route confidence trend, context hit utility.
   - Goal: tuning decisions driven by metrics, not ad-hoc inspection.
   - Done when:
     - `/diag/telemetry` includes actionable orchestration KPIs,
     - baseline vs tuned runs can be compared consistently,
     - documentation includes tuning playbook linked to KPI thresholds.

9. [ ] Live profile benchmark pack (repeatable)
   - Maintain a stable smoke/benchmark request set for `text/respond`, strict meta route, solver handoff, context reuse.
   - Track response quality + latency + retries over time.
   - Goal: regression-safe tuning cycle for production-like conditions.
   - Done when:
     - benchmark script produces comparable artifacts per run,
     - pass/fail gates are explicit for routing, schema, and output quality,
     - release checklist references this benchmark pack.
