# Router Charter

## Purpose

Ferment is a routing and dispatch kernel.

Primary objective:
- maximize answer quality per time/cost budget by composing multiple capabilities (local models, API models, and future swarm peers) under explicit contracts.

Secondary objective:
- expose stable extension points for connectivity/orchestration without expanding the trusted core.

## Operating Modes

1. Solo orchestrator (default)
- one operator, many capabilities.
- optimize latency/quality/cost via policy-aware dispatch.

2. Swarm node (optional)
- Ferment as a routing component inside Orbiplex node.
- same core semantics, different transport/connectivity adapters.

## Core Scope (must stay small)

1. Capability resolution and candidate selection.
2. Dispatch execution semantics (retry/fallback/recovery limits).
3. Quality gates and deterministic rejection taxonomy.
4. Contract validation on boundaries (request/result/handoff).
5. Minimal telemetry and replay needed for explainability.

## Non-goals for Core

1. Provider-specific feature growth in kernel.
2. Domain workflow logic unrelated to routing/dispatch.
3. Monolithic "agent platform" behavior in core namespace.
4. Hidden policy in code when data config can express it.

## Cathedral-vs-Bazaar Guardrails

Every new feature touching core must improve at least one measurable routing metric:
- dispatch precision,
- latency/SLA,
- fallback reliability,
- connectivity quality.

If not, implement as:
- optional middleware/module, or
- external adapter, or
- backlog candidate (not core).

## Stability Contract

1. Data-first contracts are source of truth.
2. Backward compatibility is preserved unless explicit migration path exists.
3. Error envelopes remain deterministic and machine-readable.
4. Policy behavior must be explainable through telemetry/replay.
5. Deferred/lazy reasoning slots (`{:slot/id ...}`) and final response compilation across capabilities are core features and must not regress.

## PR Checklist (Router-first Gate)

Copy this block into PR description:

- [ ] Change clearly improves routing/dispatch (precision, latency, reliability, or connectivity).
- [ ] Core scope unchanged or explicitly justified.
- [ ] No new provider coupling in core; integration lives in adapter/module.
- [ ] Contracts updated (request/result/error/telemetry) where behavior changed.
- [ ] Config remains data-first (no hardcoded policy if EDN can express it).
- [ ] Tests cover success + near-miss/failure path.
- [ ] Telemetry/replay visibility added for new decision points.
- [ ] Docs updated (`design.md`, `usage.md`, backlog status if applicable).

## Decision Rule

When in doubt:
- prefer smaller core + explicit extension seam,
- reject functionality that cannot be tied to router value metrics.
