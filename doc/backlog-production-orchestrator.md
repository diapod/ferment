# Production Backlog (Agent Orchestrator)

Status: new production track for orchestration maturity.
Existing tuning backlog is preserved unchanged in `doc/backlog.md`.
Completed historical work remains in `doc/backlogs-done.md`.
Execution for top priorities is tracked in `doc/execution-plan-q1.md`.

## Priority Order

1. [ ] Asynchronous execution queue with SLA controls (`#2`)
   - Add job lifecycle for `/v1/act`: `accepted -> running -> completed/failed/canceled`.
   - Add deadline, cancellation, backpressure, and priority handling.
   - Add retry policy with jitter/backoff at orchestrator level (outside model retries).
   - Done when:
     - long requests can run async and be polled/retrieved deterministically,
     - queue saturation does not block the whole node,
     - SLA breaches are explicit in telemetry and response status.

2. [ ] Deterministic replay and deep diagnostics (`#10`)
   - Add replay package: frozen request, resolved routing decision, selected candidates, and policy snapshot.
   - Add replay endpoint/tooling for post-mortem and regression diff.
   - Done when:
     - the same replay package reproduces the same execution path,
     - policy/config diffs can be compared against replay outcomes.

3. [ ] Advanced model gateway (`#3`)
   - Add per-model health/latency/error scoring.
   - Add gateway strategies: cost-aware selection, circuit breaker, optional hedging.
   - Done when:
     - routing can pick model by policy (`latency-first`, `quality-first`, `cost-first`),
     - unstable models are automatically quarantined/fallbacked.

4. [ ] Durable execution graph (`#1`)
   - Persist workflow state by node (`pending/running/succeeded/failed`).
   - Add resume after process crash/restart.
   - Done when:
     - in-flight plans can be resumed without duplicating side effects,
     - recovery path is covered by integration tests.

5. [ ] Long-term memory policy layer (`#7`)
   - Extend session memory with retrieval/summarization policy and bounded retention.
   - Add anti-contamination rules across sessions/principals.
   - Done when:
     - memory improves multi-turn consistency without unbounded growth,
     - recall behavior is explicit and testable.

6. [ ] Multi-tenant governance (`#9`)
   - Add per-tenant/user quotas, rate limits, and budget accounting.
   - Add per-tenant routing/policy overrides with safe defaults.
   - Done when:
     - token/cost/latency controls are enforced per tenant,
     - audit and telemetry are filterable by tenant/principal.

7. [ ] Continuous evaluation pipeline (`#6`)
   - Add golden task suite with latency/quality gates.
   - Add automated benchmark run as release gate.
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

## Suggested Delivery Phases

1. Phase A (stability + operability): priorities 1, 2, 3.
2. Phase B (resilience + memory): priorities 4, 5, 6.
3. Phase C (quality + governance): priorities 7, 8, 9, 10.
