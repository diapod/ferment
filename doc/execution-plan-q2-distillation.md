# Execution Plan Q2 2026 (Priority 11: Distillation Dataset Pipeline)

Scope: implementation plan for backlog item `#11` from `doc/backlog-production-orchestrator.md`.
Goal: full training-data pipeline for LoRA/QLoRA, from runtime collection to promotion gate.

## Current Baseline (already delivered)

- [x] request-level training toggle and per-request override (`:training/enabled?`),
- [x] HTTP runtime training config branch (`:training {:enabled? ... :transcript/intents ...}`),
- [x] `test-live` profile default training enablement,
- [x] replay -> `training.event/v1` exporter and LoRA JSONL exporter (`bin/export-training-events`).

## Success Criteria

- deterministic dataset build (`train/valid/test`) with reproducible manifest/snapshot id,
- strict redaction and policy labels attached to every training event,
- trainer-ready export format validated against target LoRA/QLoRA stack,
- automated post-train evaluation and promotion gate decision.

## Milestone D1: Durable Event Collection

### Implementation

- [ ] add append-only training event collector (runtime side),
- [ ] persist events to durable storage (filesystem first, pluggable backend),
- [ ] add run/session metadata for reproducibility and traceability.

### Config

- [ ] add collector config branch (`:training/collector`) under HTTP/runtime config:
  - `:enabled?`
  - `:store/type` (`:fs-jsonl` initially)
  - `:store/path`
  - `:flush-policy` (`:per-event` / `:batch`)
  - `:max-file-size-bytes` + rotation policy

### Tests

- [ ] collection survives process restart (append-only contract),
- [ ] no duplicate `training.event/id` under retry/replay scenarios,
- [ ] request-level training disable still suppresses collection consequences.

### Milestone D1 Detailed Task Breakdown (File-Level)

#### D1.1 Collector core
- [ ] `src/ferment/training/collector.clj` (new)
  - collector protocol (`append!`, `flush!`, `close!`, `stats`),
  - append-only fs-jsonl implementation with deterministic file naming,
  - event-id dedup guard (`trace/id + call-index + attempt`).
- [ ] `src/ferment/training/events.clj` (new)
  - canonical runtime event constructor (`training.event/v1`) from replay/workflow artifacts.

#### D1.2 HTTP/runtime wiring
- [ ] `src/ferment/http.clj`
  - initialize collector from config in `init-http`,
  - invoke collector after replay/event creation path,
  - emit lifecycle telemetry for collector state/errors.
- [ ] `resources/config/common/prod/http.edn`
  - add default collector branch (disabled by default).
- [ ] `resources/config/common/test-live/training.edn`
  - enable collector defaults for smoke-live training path.

#### D1.3 Integration tests
- [ ] `test/ferment/http_test.clj`
  - collector append on successful training request,
  - explicit `:training/enabled? false` disables append,
  - explicit transcript disable does not break collector contract.
- [ ] `test/ferment/training/collector_test.clj` (new)
  - fs append semantics, rotation behavior, idempotency.

## Milestone D2: Critic/Judge Labels + Redaction

### Implementation

- [ ] add constitution-aware critic labels per event:
  - pass/fail,
  - per-rule verdicts,
  - score + reject/repair reason taxonomy.
- [ ] add dedicated redaction pipeline (beyond replay marker checks):
  - PII/secret scrub policy,
  - allowlist/denylist selectors,
  - deterministic replacement policy.

### Config

- [ ] add `:training/judge` and `:training/redaction` branches:
  - judge mode (`:disabled` / `:teacher` / `:rules-only`),
  - constitution/rules reference,
  - redaction key paths and pattern rules.

### Tests

- [ ] secrets/PII are removed from all exported branches (`request`, `call`, `out`, `meta`),
- [ ] judge labels are stable and reproducible for fixed input snapshot,
- [ ] reject/repair taxonomy is present in training event labels.

### Milestone D2 Detailed Task Breakdown (File-Level)

#### D2.1 Judge labels
- [ ] `src/ferment/training/judge.clj` (new)
  - critic API (`evaluate!`) returning normalized verdict map,
  - canonical label mapping into `training.event/v1`.
- [ ] `src/ferment/training/events.clj`
  - include `:labels/:judge/*` and constitution references.

#### D2.2 Redaction
- [ ] `src/ferment/training/redaction.clj` (new)
  - path-based and pattern-based redaction combinators,
  - deterministic scrub rules with audit counters.
- [ ] `src/ferment/training/export_events.clj`
  - apply strict redaction pass before writing events/train rows.

#### D2.3 Tests
- [ ] `test/ferment/training/redaction_test.clj` (new)
- [ ] `test/ferment/training/judge_test.clj` (new)
- [ ] extend `test/ferment/training/export_events_test.clj`
  - judge labels and redaction contract assertions.

## Milestone D3: Deterministic Dataset Build + Trainer-Ready Export

### Implementation

- [ ] add dataset builder pipeline:
  - deterministic partitioning (`train/valid/test`),
  - manifest (`counts`, `hashes`, `filters`, `time window`, `config snapshot`),
  - dedup and filtering policy.
- [ ] export trainer-ready rows:
  - chat/messages template and/or canonical text template per target stack,
  - tokenization sanity pre-check hooks.

### Config

- [ ] add `:training/dataset` and `:training/export` branches:
  - split ratios/seed,
  - include-failed policy,
  - target format (`:chatml`, `:messages`, `:sft-prompt-completion`),
  - output paths + artifact naming.

### Tests

- [ ] repeated build over identical input yields identical manifest + file hashes,
- [ ] split isolation by stable key (no cross-split leakage),
- [ ] exported rows pass schema checks for target trainer format.

### Milestone D3 Detailed Task Breakdown (File-Level)

#### D3.1 Dataset builder
- [ ] `src/ferment/training/dataset.clj` (new)
  - deterministic split and manifest generation,
  - snapshot id/hash utilities.
- [ ] `bin/build-training-dataset` (new)
  - CLI wrapper for collector/export/dataset build flow.

#### D3.2 Export format adapters
- [ ] `src/ferment/training/export_events.clj`
  - add target-format encoder options (`messages`/`chat template`),
  - preserve existing `training.event/v1` output as canonical source artifact.
- [ ] `test/ferment/training/export_events_test.clj`
  - add format-specific regression tests.

#### D3.3 Documentation
- [ ] `doc/usage.md`
  - end-to-end commands for dataset build and artifact verification.

## Milestone D4: Evaluation Suite + Promotion Gate

### Implementation

- [ ] add offline eval runner for student checkpoints:
  - protocol conformance,
  - constitution compliance,
  - regression task suite.
- [ ] define promotion gate policy:
  - threshold config,
  - pass/fail report artifact,
  - explicit rejection reasons.

### Config

- [ ] add `:training/eval` and `:training/promotion` branches:
  - metric thresholds,
  - required suites,
  - blocking/non-blocking criteria.

### Tests

- [ ] gate rejects candidate below threshold and records reason,
- [ ] gate accepts candidate above threshold with deterministic report schema,
- [ ] regression suite includes protocol edge cases and high-risk prompts.

### Milestone D4 Detailed Task Breakdown (File-Level)

#### D4.1 Eval + gate core
- [ ] `src/ferment/training/eval.clj` (new)
  - suite runner and metric aggregator.
- [ ] `src/ferment/training/promotion.clj` (new)
  - gate decision logic and report artifact generator.
- [ ] `bin/eval-student` (new)
  - CLI invoker for eval + gate pass/fail.

#### D4.2 Tests + docs
- [ ] `test/ferment/training/eval_test.clj` (new)
- [ ] `test/ferment/training/promotion_test.clj` (new)
- [ ] `doc/backlog-production-orchestrator.md`
  - sync delivery status for `#11` milestones.

## Rollout Sequence

1. Week 1-2: Milestone D1 (durable collector + runtime wiring).
2. Week 3: Milestone D2 (judge labels + redaction).
3. Week 4: Milestone D3 (dataset build + trainer-ready export).
4. Week 5: Milestone D4 (evaluation suite + promotion gate).
5. Week 6: hardening, reproducibility audit, and release decision.

## Release Gate Checklist (Priority #11)

- [ ] `bin/test --focus ferment.training-` suites green
- [ ] deterministic rebuild reproduces manifest + hashes
- [ ] redaction audit passes (no secret/PII leakage in artifacts)
- [ ] trainer dry-run validates exported dataset format
- [ ] eval + promotion report generated and archived for candidate model
