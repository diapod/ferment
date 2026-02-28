# Execution Plan Q1 2026 (Priorities 1-3)

Scope: production orchestrator track, aligned with priority order from `doc/backlog-production-orchestrator.md`.
Window: remaining Q1 2026 delivery cycle.

## Targets (Operational + Latency)

- `text/respond` in `routing.profile=low-latency`:
  - p50 <= 6s
  - p95 <= 10s
  - error rate (`5xx`) < 1%
- strict meta route (`meta?=true`, `strict?=true`, `force?=true`):
  - p95 <= 20s
  - fail-closed diagnostics completeness: 100% (`failure/type`, `switch-on`, `retry-policy`, `candidates`)
- queue mode (`async`):
  - API accept latency p95 <= 300ms
  - no starvation under load (priority + backpressure active)

## Milestone A (P1): Async Queue + SLA Controls

### Implementation
- [ ] Add async mode for `/v1/act` with job lifecycle:
  - `accepted -> running -> completed|failed|canceled`
- [ ] Add endpoints:
  - `POST /v1/act` (`response/type=accepted` -> job id)
  - `GET /v1/act/jobs/:id`
  - `POST /v1/act/jobs/:id/cancel`
- [ ] Add scheduler controls:
  - deadline/timeout per job
  - priority classes (`interactive`, `batch`)
  - queue limits + backpressure policy
  - jittered retry for transient orchestrator/runtime failures

### Config
- [ ] Add queue branch in runtime config:
  - `resources/config/common/prod/runtime.edn`
  - `resources/config/common/test-live/runtime.edn` (if needed as overlay)
- [ ] Add SLA defaults per intent in protocol/router config:
  - max deadline, cancelability, queue class

### Tests
- [ ] HTTP integration: submit -> poll -> complete flow
- [ ] Cancellation test: running job transitions to `canceled`
- [ ] Backpressure test: queue full returns deterministic overload response
- [ ] Retry with jitter test: transient failure recovers within policy bounds

### Milestone A Detailed Task Breakdown (File-Level)

#### A1. Queue domain + contracts
- [x] `src/ferment/contracts.clj`
  - add canonical shapes for:
    - accepted response envelope (`job/id`, `job/status`, `submitted-at`, optional `deadline-at`)
    - job status payload (`queued|running|completed|failed|canceled|expired`)
    - cancel response payload
  - add validation helpers reused by HTTP and runtime.
- [x] `test/ferment/contracts_test.clj`
  - add positive/negative coverage for accepted/status/cancel payloads.

#### A2. Queue service (in-memory first, pluggable later)
- [x] `src/ferment/queue.clj` (new namespace)
  - implement queue protocol:
    - `submit!`, `poll!`, `start-next!`, `complete!`, `fail!`, `cancel!`, `get-job`
  - enforce queue capacity and deterministic overflow behavior
  - enforce deadline and state transitions (no invalid jumps)
  - emit transition events for telemetry/audit.
- [x] `src/ferment/runtime.clj`
  - wire queue service into runtime map under a stable key (for HTTP + workers).

#### A3. Async worker loop + SLA controls
- [x] `src/ferment/runtime.clj` (or `src/ferment/queue.clj`, depending on final split)
  - add worker loop consuming queued jobs and invoking existing act flow
  - add per-job timeout/deadline enforcement
  - add retry policy (bounded, jittered) for transient failures
  - ensure idempotent completion/failure writes.
- [x] `src/ferment/telemetry.clj`
  - add queue counters:
    - `jobs/submitted`, `jobs/started`, `jobs/completed`, `jobs/failed`, `jobs/canceled`, `jobs/expired`
    - queue depth and wait-time histogram inputs.

#### A4. HTTP surface
- [x] `src/ferment/http.clj`
  - extend `/v1/act`:
    - `response/type=accepted` submits async job and returns accepted envelope
    - default path keeps current sync behavior
  - add endpoints:
    - `GET /v1/act/jobs/:id`
    - `POST /v1/act/jobs/:id/cancel`
  - normalize errors:
    - queue full -> deterministic overload response
    - unknown job -> canonical not-found response.
- [ ] `src/ferment/app.clj`
  - ensure routes are mounted in all relevant profiles.

#### A5. Configuration
- [ ] `resources/config/common/prod/runtime.edn`
  - add queue branch:
    - `enabled?`, `max-size`, `workers`, `default-timeout-ms`, `max-deadline-ms`
    - retry defaults (`max-attempts`, `base-backoff-ms`, `jitter-ms`).
- [ ] `resources/config/common/test-live/runtime.edn`
  - add conservative queue defaults for live smoke.
- [ ] `resources/config/common/prod/protocol.edn`
  - add intent-level async/SLA defaults:
    - `queue/class`, `cancelable?`, `deadline-ms` (where needed).

#### A6. Tests (integration + concurrency)
- [ ] `test/ferment/http_test.clj`
  - accepted submit -> poll -> completed
  - cancel pending/running job
  - queue full behavior and status code mapping
  - idempotent poll on terminal states.
- [x] `test/ferment/runtime_test.clj` (new or existing)
  - worker loop lifecycle and timeout handling
  - retry with jitter bounded by config
  - no invalid state transitions under race.
- [ ] `test/ferment/telemetry_test.clj` (new or existing)
  - queue counters and latency buckets update deterministically.

#### A7. Operational playbook
- [ ] `doc/usage.md`
  - document async usage examples (`submit`, `poll`, `cancel`)
  - document queue/SLA knobs and recommended defaults.
- [ ] Smoke script update (existing benchmark pack)
  - add async scenario with pass/fail thresholds:
    - accept latency p95 <= 300ms
    - completion within configured deadline for representative prompts.

## Milestone B (P2): Deterministic Replay + Deep Diagnostics

### Implementation
- [ ] Add replay package persisted per execution:
  - frozen request envelope
  - effective routing config
  - selected candidates and node outcomes
  - policy snapshot/version id
- [ ] Add replay read API:
  - `GET /v1/act/replay/:trace-id` (or equivalent internal admin endpoint)
- [ ] Add diff utility:
  - compare replay outcomes between policy/model versions

### Config
- [ ] Add replay retention config (TTL + storage cap)
- [ ] Add redaction policy for sensitive payload sections

### Tests
- [ ] Determinism test: replay reproduces same path with frozen inputs
- [ ] Redaction test: secrets are never exposed in replay payload
- [ ] Diff test: changed policy yields explicit path delta report

## Milestone C (P3): Advanced Model Gateway

### Implementation
- [ ] Add per-model health registry:
  - rolling latency/error window
  - availability status
- [ ] Add selection strategies:
  - `latency-first`
  - `quality-first`
  - `cost-first`
- [ ] Add resilience patterns:
  - circuit breaker per model endpoint
  - optional hedging for selected intents (guarded by config)

### Config
- [ ] Extend model/router config with strategy and thresholds:
  - breaker thresholds (error %, cooldown)
  - hedge timeout and max parallel probes
  - strategy per intent/profile

### Tests
- [ ] Router test: strategy chooses expected model candidate
- [ ] Gateway test: breaker opens after threshold and routes away
- [ ] Hedge test: slower candidate is canceled/ignored after winner selected

## Rollout Sequence

1. Week 1-2: Milestone A (queue + SLA skeleton, endpoints, basic tests)
2. Week 3-4: Milestone B (replay package + replay API + deterministic tests)
3. Week 5-6: Milestone C (gateway strategy + breaker, optional hedge)
4. Week 7: hardening, benchmark run, and release gate decision

## Release Gate Checklist

- [ ] `bin/test` green
- [ ] benchmark pack run completed (baseline vs candidate)
- [ ] latency targets met for low-latency profile
- [ ] strict fail-closed diagnostics complete
- [ ] replay retrieval and redaction verified
- [ ] gateway breaker behavior verified under synthetic failure
