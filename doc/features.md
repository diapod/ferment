# Ferment Features

This document describes core features currently available in Ferment.

## 1. Data-first contracts

- Unified request/response envelope based on EDN/JSON maps (`:proto`, `:task`, `:input`, `:context`, `:done`, `:budget`, `:effects`).
- Explicit contracts on boundaries (input/output schemas, validation, error taxonomy).
- Deterministic execution shape where possible: same intent + constraints should yield the same orchestration path.

## 2. Capability-centric architecture

- Everything is executed as a capability (`:cap/id` + metadata), not as hardcoded provider calls.
- Uniform invocation semantics across executors: `invoke(cap, ctx, req) -> result`.
- Same orchestration model for LLM and non-LLM components.

## 3. Plan-as-data orchestration (IR)

- Workflows are represented as data (EDN plan/AST), not hidden control flow.
- Native support for call nodes and recursive delegation (`Value | Plan | Stream` result forms).
- Declarative delegation by `:intent`, `:requires`, and `:dispatch` policies.

## 4. Quality-aware dispatch and resilience

- Late binding of executor selection at runtime.
- Retry/fallback/switch policies in workflow execution.
- Per-intent quality gates (`:must`, `:should`, `:score-min`) with optional judge capability.

## 5. Stratified modules and boundaries

- Clear split of layers: domain -> orchestration -> adapters -> runtime.
- Side effects isolated in adapters.
- Thin module interfaces based on maps and stable keys.

## 6. Layered configuration and profile model

- Deterministic config loading from `common` + `local` overlays.
- Profile set: `prod`, `dev`, `admin`, `test`, `test-live`.
- Runtime bootstrap with profile-specific app entrypoints (`start!`, `start-dev!`, `start-admin!`, `start-test!`, `start-test-live!`).

## 7. HTTP bridge and API endpoints

- Public operational endpoints: `/v1/act`, `/v1/session`, `/v1/admin`, `/health`, `/routes`, `/diag/telemetry`.
- `/v1/act` uses compiled middleware pipeline with explicit module contract.
- Optional response cache for `/v1/act` with TTL and size limits.

## 8. Session state and working memory

- Session lifecycle operations and worker controls (`open`, `freeze`, `thaw`, `state`).
- Session vars CRUD with policy by intent/operation.
- Namespace-classified vars with TTL/freeze controls and default request bindings.

## 9. Security and effect control

- Role-based policy checks for runtime/admin actions.
- Effect allowlist contract (`:effects :allowed`) and runtime scope enforcement.
- Tool invocation carries auth/roles context and re-checks effect authorization.

## 10. Model runtime flexibility

- Runtime supports process-based invoke (stdin) and HTTP invoke transports.
- Per-role model selection and runtime branches in config.
- Worker lifecycle controls (freeze/thaw/TTL) to manage resources.

## 11. Observability and operations

- Structured telemetry snapshots via `/diag/telemetry`.
- Quality and orchestration KPI (parse/retry/fallback/must-failed/judge-pass/cache-hit, route trends, participant diversity).
- Built-in DB migration/rollback helpers and benchmark packs for live tuning.

## 12. Developer workflow support

- REPL-first local development (`bin/repl`).
- Standard test paths (`bin/test`, `bin/test-full`, `make test`, `make test-full`).
- Lint/docs/build helpers (`make lint`, `make docs`, `make jar`, `make pom`).
