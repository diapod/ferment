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

- [x] add append-only training event collector (runtime side),
- [x] persist events to durable storage (filesystem first, pluggable backend),
- [x] add run/session metadata for reproducibility and traceability.

### Config

- [x] add collector config branch (`:training/collector`) under HTTP/runtime config:
  - `:enabled?`
  - `:store/type` (`:fs-jsonl` initially)
  - `:store/path`
  - `:flush-policy` (`:per-event` / `:batch`)
  - `:max-file-size-bytes` + rotation policy

### Tests

- [x] collection survives process restart (append-only contract),
- [x] no duplicate `training.event/id` under retry/replay scenarios,
- [x] request-level training disable still suppresses collection consequences.

### Milestone D1 Detailed Task Breakdown (File-Level)

#### D1.1 Collector core
- [x] `src/ferment/training/collector.clj` (new)
  - collector protocol (`append!`, `flush!`, `close!`, `stats`),
  - append-only fs-jsonl implementation with deterministic file naming,
  - event-id dedup guard (`trace/id + call-index + attempt`).
- [x] `src/ferment/training/events.clj` (new)
  - canonical runtime event constructor (`training.event/v1`) from replay/workflow artifacts.

#### D1.2 HTTP/runtime wiring
- [x] `src/ferment/http.clj`
  - initialize collector from config in `init-http`,
  - invoke collector after replay/event creation path,
  - emit lifecycle telemetry for collector state/errors.
- [x] `resources/config/common/prod/http.edn`
  - add default collector branch (disabled by default).
- [x] `resources/config/common/test-live/training.edn`
  - enable collector defaults for smoke-live training path.

#### D1.3 Integration tests
- [x] `test/ferment/http_test.clj`
  - collector append on successful training request,
  - explicit `:training/enabled? false` disables append,
  - explicit transcript disable does not break collector contract.
- [x] `test/ferment/training/collector_test.clj` (new)
  - fs append semantics, rotation behavior, idempotency.

## Milestone D2: Critic/Judge Labels + Redaction

### Implementation

- [x] add constitution-aware critic labels per event:
  - pass/fail,
  - per-rule verdicts,
  - score + reject/repair reason taxonomy.
- [x] add dedicated redaction pipeline (beyond replay marker checks):
  - PII/secret scrub policy,
  - allowlist/denylist selectors,
  - deterministic replacement policy.

### Config

- [x] add `:training/judge` and `:training/redaction` branches:
  - judge mode (`:disabled` / `:teacher` / `:rules-only`),
  - constitution/rules reference,
  - redaction key paths and pattern rules.

### Tests

- [x] secrets/PII are removed from all exported branches (`request`, `call`, `out`, `meta`),
- [x] judge labels are stable and reproducible for fixed input snapshot,
- [x] reject/repair taxonomy is present in training event labels.

### Milestone D2 Detailed Task Breakdown (File-Level)

#### D2.1 Judge labels
- [x] `src/ferment/training/judge.clj` (new)
  - critic API (`evaluate!`) returning normalized verdict map,
  - canonical label mapping into `training.event/v1`.
- [x] `src/ferment/training/events.clj`
  - include `:labels/:judge/*` and constitution references.

#### D2.2 Redaction
- [x] `src/ferment/training/redaction.clj` (new)
  - path-based and pattern-based redaction combinators,
  - deterministic scrub rules with audit counters.
- [x] `src/ferment/training/export_events.clj`
  - apply strict redaction pass before writing events/train rows.

#### D2.3 Tests
- [x] `test/ferment/training/redaction_test.clj` (new)
- [x] `test/ferment/training/judge_test.clj` (new)
- [x] extend `test/ferment/training/export_events_test.clj`
  - judge labels and redaction contract assertions.

## Milestone D3: Deterministic Dataset Build + Trainer-Ready Export

### Implementation

- [x] add dataset builder pipeline:
  - deterministic partitioning (`train/valid/test`),
  - manifest (`counts`, `hashes`, `filters`, `time window`, `config snapshot`),
  - dedup and filtering policy.
- [x] export trainer-ready rows:
  - chat/messages template and/or canonical text template per target stack,
  - tokenization sanity pre-check hooks.

### Config

- [x] add `:training/dataset` and `:training/export` branches:
  - split ratios/seed,
  - include-failed policy,
  - target format (`:chatml`, `:messages`, `:sft-prompt-completion`),
  - output paths + artifact naming.

### Tests

- [x] repeated build over identical input yields identical manifest + file hashes,
- [x] split isolation by stable key (no cross-split leakage),
- [x] exported rows pass schema checks for target trainer format.

### Milestone D3 Detailed Task Breakdown (File-Level)

#### D3.1 Dataset builder
- [x] `src/ferment/training/dataset.clj` (new)
  - deterministic split and manifest generation,
  - snapshot id/hash utilities.
- [x] `bin/build-training-dataset` (new)
  - CLI wrapper for collector/export/dataset build flow.

#### D3.2 Export format adapters
- [x] `src/ferment/training/export_events.clj`
  - add target-format encoder options (`messages`/`chat template`),
  - preserve existing `training.event/v1` output as canonical source artifact.
- [x] `test/ferment/training/export_events_test.clj`
  - add format-specific regression tests.

#### D3.3 Documentation
- [x] `doc/usage.md`
  - end-to-end commands for dataset build and artifact verification.

## Milestone D4: Evaluation Suite + Promotion Gate

### Implementation

- [x] add offline eval runner for student checkpoints:
  - protocol conformance,
  - constitution compliance,
  - regression task suite.
- [x] define promotion gate policy:
  - threshold config,
  - pass/fail report artifact,
  - explicit rejection reasons.

### Config

- [x] add `:training/eval` and `:training/promotion` branches:
  - metric thresholds,
  - required suites,
  - blocking/non-blocking criteria.

### Tests

- [x] gate rejects candidate below threshold and records reason,
- [x] gate accepts candidate above threshold with deterministic report schema,
- [x] regression suite includes protocol edge cases and high-risk prompts.

### Milestone D4 Detailed Task Breakdown (File-Level)

#### D4.1 Eval + gate core
- [x] `src/ferment/training/eval.clj` (new)
  - suite runner and metric aggregator.
- [x] `src/ferment/training/promotion.clj` (new)
  - gate decision logic and report artifact generator.
- [x] `bin/eval-student` (new)
  - CLI invoker for eval + gate pass/fail.

#### D4.2 Tests + docs
- [x] `test/ferment/training/eval_test.clj` (new)
- [x] `test/ferment/training/promotion_test.clj` (new)
- [x] `doc/backlog-production-orchestrator.md`
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
