# Production Backlog (Agent Orchestrator)

Status: new production track for orchestration maturity.
Existing tuning backlog is preserved in `doc/backlog.md` and the #2-#9 tuning package is marked delivered (sync: 2026-02-28).
Completed historical work remains in `doc/backlogs-done.md`.
Execution for top priorities is tracked in:
- `doc/execution-plan-q1.md` (priorities 1-3),
- `doc/execution-plan-q2-distillation.md` (priority 11).
Reprioritized for router-first meta-goal (sync: 2026-03-09): keep the core small, maximize routing/dispatch quality, and prioritize heterogeneous model connectivity.

## Active Priority Queue (Router-first)

1. [x] `#11` Distillation dataset pipeline for LoRA/QLoRA (operational close-out)
   - Closed: 2026-03-09.
   - Release-gate artifacts:
     - `target/training/release-gate-20260309T163935Z`
     - `target/training/release-gate-20260309T163935Z/reports/release-gate-summary.json`

2. [x] `#6` Continuous evaluation pipeline
   - Closed: 2026-03-09.
   - Delivery:
     - benchmark release gate comparator `bin/benchmark-gate` (candidate-only and baseline-vs-candidate modes),
     - deterministic gate artifacts (`gate-report.json`, `gate-report.md`) with blocking exit code for CI,
     - documentation of CI-ready flow in `doc/usage.md`.

3. [x] `#5` Prompt/policy versioning and controlled rollout
   - Reason: safe, reversible tuning loop for router behavior without core churn.
   - Focus now:
     - [x] protocol artifact selector foundation (`:versions`, `:rollout`, deterministic canary by `trace.id`, request override),
     - [x] artifact versioning for `prompts`, `policy/intents`, routing strategy (runtime selection for protocol + router with per-request override),
     - [x] canary rollout + one-step rollback (runtime override via admin actions),
     - [x] shadow rollout execution + side-by-side outcome comparison.
   - Note: long-tail extension `#5` in the lower priority section still tracks optional A/B variants.

4. [ ] `#12` Heterogeneous connectivity adapters (API + swarm-ready)
   - Reason: Ferment meta-purpose requires excellent connectivity to local, remote API, and future decentralized peers.
   - Scope:
     - standard capability descriptor for local/API/peer model endpoints,
     - adapter seam in gateway/resolver (no provider coupling in core),
     - health/auth/timeout/retry normalization per transport.
   - Done when:
     - one intent can route across at least two transport classes (local + remote API) by policy,
     - telemetry clearly distinguishes transport failures from model-quality failures.

5. [ ] `#4` Full runtime isolation for tools/effects
   - Reason: required safety boundary as connectivity surface expands.

6. [ ] `#8` Human-in-the-loop checkpoints
   - Reason: operational control for high-risk side effects after isolation policies are in place.

## Priority Order

1. [x] Asynchronous execution queue with SLA controls (`#2`)
   - Add job lifecycle for `/v1/act`: `accepted -> running -> completed/failed/canceled`.
   - Add deadline, cancellation, backpressure, and priority handling.
   - Add retry policy with jitter/backoff at orchestrator level (outside model retries).
   - Delivery status (synced: 2026-03-01):
     - [x] async lifecycle + endpoints (`/v1/act` accepted, job status, job cancel),
     - [x] in-memory queue workers with deadline/timeout, cancel, priority, backpressure,
     - [x] orchestrator retry with jitter/backoff for transient queue execution failures,
     - [x] queue telemetry counters and HTTP/runtime regression tests,
     - [x] explicit profile-level queue config overlays (`runtime.edn`) + operator usage section for async flow.
   - Done when:
     - long requests can run async and be polled/retrieved deterministically,
     - queue saturation does not block the whole node,
     - SLA breaches are explicit in telemetry and response status.

2. [x] Deterministic replay and deep diagnostics (`#10`)
   - Add replay package: frozen request, resolved routing decision, selected candidates, and policy snapshot.
   - Add replay endpoint/tooling for post-mortem and regression diff.
   - Delivery status (synced: 2026-03-01):
     - [x] replay package capture on `/v1/act` (request/routing/policy/response/auth/timing),
     - [x] deep diagnostics branch in replay (`execution-path`, telemetry `before/after/delta`),
     - [x] replay endpoint (`/v1/act/replay/{trace-id}`) + compare mode (`?against=<trace-id>`),
     - [x] deterministic re-execution from replay package (`POST /v1/act/replay/{trace-id}/rerun`) + path comparison,
     - [x] automated policy/config diff report against replay baseline.
   - Done when:
     - the same replay package reproduces the same execution path,
     - policy/config diffs can be compared against replay outcomes.

3. [x] Advanced model gateway (`#3`)
   - Add per-model health/latency/error scoring.
   - Add gateway strategies: cost-aware selection, circuit breaker, optional hedging.
   - Delivery status (sync: 2026-03-01):
     - [x] runtime model-health registry (`:gateway/model-health`) wired to resolver/workflow,
     - [x] candidate ranking strategies (`:latency-first`, `:quality-first`, `:cost-first`) via `:routing/:gateway`,
     - [x] circuit-breaker quarantine on open circuits (`:gateway/circuit-open`) with cooldown,
     - [x] optional hedging execution (parallel probes + winner selection).
   - Done when:
     - routing can pick model by policy (`latency-first`, `quality-first`, `cost-first`),
     - unstable models are automatically quarantined/fallbacked.

4. [x] Durable execution graph (`#1`)
   - Persist workflow state by node (`pending/running/succeeded/failed`).
   - Add resume after process crash/restart.
   - Delivery status (sync: 2026-03-05):
     - [x] durable node lifecycle events with persisted checkpoints (`:node/succeeded` + `:checkpoint`),
     - [x] runtime startup recovery from execution-graph into async queue (`resume-queued-jobs!`),
     - [x] tool-node pre-commit checkpoint before outer loop finalization (prevents side-effect re-run on crash window),
     - [x] regression coverage for crash/resume and runtime restore (`ferment.workflow-test`, `ferment.runtime-test`, `ferment.execution-graph-test`).
   - Done when:
     - in-flight plans can be resumed without duplicating side effects,
     - recovery path is covered by integration tests.

5. [x] Long-term memory policy layer (`#7`)
   - Extend session memory with retrieval/summarization policy and bounded retention.
   - Add anti-contamination rules across sessions/principals.
   - Delivery status (synced: 2026-03-06):
     - [x] intent-aware memory read/write policy (`:memory/policy` with `:read/by-intent`, `:write/by-intent`),
     - [x] principal-isolation guard for context recall (`:principal/isolation?`, `:principal/key`) with telemetry counter for blocked recalls,
     - [x] bounded history retention (`:history/enabled?`, `:history/key`, `:history/max-items`) on memory auto-write,
     - [x] regression tests for policy normalization, recall gating, and bounded history persistence.
   - Done when:
     - memory improves multi-turn consistency without unbounded growth,
     - recall behavior is explicit and testable.

6. [x] Multi-tenant governance (`#9`)
   - Add per-tenant/user quotas, rate limits, and budget accounting.
   - Add per-tenant routing/policy overrides with safe defaults.
   - Delivery status (synced: 2026-03-09):
     - [x] tenant-aware request shaping (`:routing` defaults + budget/timeout clamps) from runtime tenancy policy,
     - [x] pre-execution tenant/principal guardrails for rpm, concurrency, and daily billed-token budget,
     - [x] per-request accounting in tenancy state (requests/errors/latency/billed tokens + reject reasons),
     - [x] telemetry tenancy branch with tenant/principal filters (`/diag/telemetry?tenant=...&principal=...`),
     - [x] audit logs enriched with `:tenant-id` and `:principal-ref`.
   - Done when:
     - token/cost/latency controls are enforced per tenant,
     - audit and telemetry are filterable by tenant/principal.

7. [x] Continuous evaluation pipeline (`#6`)
   - Add golden task suite with latency/quality gates.
   - Add automated benchmark run as release gate.
   - Delivery status (sync: 2026-03-09):
     - [x] canonical golden case packs (`resources/bench/act*`) already used by `bin/benchmark-live`,
     - [x] automated release-gate runner `bin/benchmark-gate` with deterministic pass/fail report and non-zero exit on gate failure,
     - [x] baseline-vs-candidate regression checks for latency/quality drift (`interactive p95`, `must-failed SLA`, truncated output growth).
   - Done when:
     - config/prompt/routing changes are blocked on failing quality or latency gates,
     - baseline vs candidate reports are generated automatically.

8. [ ] Prompt/policy versioning and controlled rollout (`#5`)
   - Version `prompts`, `policy/intents`, and routing strategy as deployable artifacts.
   - Add canary, A/B, shadow run, and fast rollback.
   - Done when:
     - runtime can switch versions without code changes,
     - rollback is one-step and auditable.

9. [ ] Full runtime isolation for tools/effects (`#4`)
   - Enforce tool sandboxing (CPU/memory/time/fs/net) per call.
   - Add secret scoping per capability and per principal.
   - Done when:
     - effect execution cannot escape declared scope,
     - denied/violating operations are blocked and logged deterministically.

10. [ ] Human-in-the-loop checkpoints (`#8`)
   - Add approval gates for high-risk actions (`fs/write`, `db/write`, `process/run`, external side effects).
   - Add approval timeout/escalation and explicit audit trace.
   - Done when:
     - sensitive actions require explicit approval policy,
     - non-approved flows fail closed with clear operator feedback.

11. [x] Distillation dataset pipeline for LoRA/QLoRA (`#11`)
   - Extend current replay/export flow into a durable, reproducible training-data pipeline.
   - Delivery scope:
     - persist training events in append-only storage (not only in-memory replay TTL cache),
     - add critic/judge labels (constitution checks, score, reject/repair reason),
     - add strict redaction/PII scrubbing before dataset export,
     - support deterministic dataset build (`train/valid/test` split + manifest + snapshot hash),
     - export training rows in trainer-ready format (chat template/messages and/or canonical text format),
     - add post-train evaluation suite and promotion gate criteria.
   - Delivery status (sync: 2026-03-09):
     - [x] durable append-only training collector + export event stream tooling,
     - [x] critic/judge labels in canonical `training.event/v1`,
     - [x] redaction/PII scrubbing before export/build,
     - [x] deterministic dataset builder with manifest/hash/idempotent incremental append,
     - [x] trainer-ready export adapters (`sft-prompt-completion`, `messages`, `chatml`),
     - [x] offline eval runner + promotion gate CLI (`bin/eval-student`) with deterministic decision reasons,
     - [x] release gate checklist closed with archived artifacts (`target/training/release-gate-20260309T163935Z`).
   - Done when:
     - the same input snapshot reproduces identical dataset artifacts,
     - exported datasets are directly consumable by target LoRA/QLoRA trainers,
     - promotion decision is automated by explicit quality/latency regression thresholds.

12. [ ] Heterogeneous connectivity adapters (`#12`)
   - Add transport-agnostic adapter layer for non-local model execution (HTTP API now, peer/swarm later).
   - Delivery scope:
     - define canonical capability transport descriptor (`:transport/type`, auth mode, timeouts, retry limits),
     - add adapter implementations for local runtime and remote HTTP API under shared gateway contract,
     - normalize transport-level failures into deterministic taxonomy (separate from quality/schema failures),
     - preserve router-first core boundary (provider specifics only in adapters/modules).
   - Done when:
     - resolver/gateway can choose candidates across local + remote transports by policy,
     - telemetry exposes transport class and failure class with stable counters,
     - no provider-specific branching is added inside orchestration core flow.

## Suggested Delivery Phases

1. Phase A (stability + operability): priorities 1, 2, 3.
2. Phase B (resilience + memory): priorities 4, 5, 6.
3. Phase C (quality + governance): priorities 7, 8, 9, 10, 11.
