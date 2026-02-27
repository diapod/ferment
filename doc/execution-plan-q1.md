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

