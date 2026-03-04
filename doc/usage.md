# Use Cases

## Preparation

Use this checklist before any runtime scenario.

1. Select a profile.
   - Typical local development profile: `dev`
   - Other common profiles: `prod`, `admin`, `test`, `test-live`

2. Configure model/cache directories in `locations.env`.
   - Shared defaults: `resources/config/common/prod/locations.env`
   - Profile overlay example: `resources/config/common/dev/locations.env`
   - Machine-local overrides: `resources/config/local/<profile>/locations.env`

3. Configure database variables in `db.env`.
   - Base local defaults (tracked in repo): `resources/config/local/prod/db.env`
   - Profile overlays (optional): `resources/config/common/<profile>/db.env`
   - Machine-local profile overrides (optional): `resources/config/local/<profile>/db.env`
   - Effective shell merge order used by helper scripts: `common/prod -> local/prod -> common/<profile> -> local/<profile>`

4. Install local dependencies (tooling + model CLI).

```bash
bin/setup
```

5. Create database and DB users (interactive passwords unless provided in env).

```bash
# defaults to dev
bin/create-db-user

# explicit profile
bin/create-db-user dev
```

6. Apply migrations.

```bash
# defaults to dev
bin/migrate-db

# explicit profile
bin/migrate-db dev
```

7. (Optional) Roll back migration(s).

```bash
# one step back
bin/rollback-db dev

# by count
bin/rollback-db dev 2

# by migration id
bin/rollback-db dev 0004-roles
```

8. Start the application (full profile system) in REPL.

```clojure
(require '[ferment.app :as app])
(app/start-dev!)
```

9. Smoke-check HTTP bridge (when `:ferment.http/default` is running).

```bash
curl -s http://127.0.0.1:12002/health
curl -s http://127.0.0.1:12002/routes
curl -s http://127.0.0.1:12002/diag/telemetry
```

`/diag/telemetry` includes canonical quality KPI under `telemetry.kpi`:
- `parse-rate`
- `retry-rate`
- `fallback-rate`
- `must-failed-rate`
- `judge-pass-rate`
- `cache-hit-rate`
- `failure-taxonomy` (`by-type` + `by-domain`)

Lifecycle observability is available under `telemetry.lifecycle` (`total`, `errors`, per-component transitions, recent events window).

Orchestration tuning KPI is available under `telemetry.orchestration`:
- `participants/diversity`
- `route/decision-quality-trend`
- `context/hit-utility`

Optional `/v1/act` response cache is configured in `resources/config/common/prod/http.edn` under `:response-cache`:
- `:enabled?` (default `false`)
- `:ttl-ms`
- `:max-size`

Optional `/v1/act` middleware chain is configured in `resources/config/common/prod/http.edn` under `:act/middleware`:
- each entry is a `#ref` to a middleware module key,
- each middleware module must resolve to a map with `:name` and `:compile`,
- chain is compiled at HTTP startup (`:ferment.http/default`) and then used for every `/v1/act` request.

Default chain in production config:
- `#ref :ferment.http.act.middleware/prepare`
- `#ref :ferment.http.act.middleware/route`
- `#ref :ferment.http.act.middleware/execute`
- `#ref :ferment.http.act.middleware/finalize`

Prompt tuning source of truth:
- `resources/config/common/prod/protocol.edn`
  - `:prompts/:default`
  - `:prompts/:roles`
  - `:prompts/:intents`
- intent-level `:system` / `:system/prompt` still work as explicit full override.

## 1) Local single-model chat (quickest path)

Use this when you want to test one model directly, outside orchestration.

```bash
FERMENT_PROFILE=dev bin/chat mlx-community/SmolLM3-3B-8bit
```

Use `bin/chat <model-id> [mlx_lm.chat args...]` for ad-hoc diagnostics.

## 2) Start only selected runtime branches

Use this when you want partial startup instead of full `start-dev!`.

```clojure
(require '[ferment.app :as app])

;; Start HTTP + its dependencies (runtime/models/session/etc.)
(app/start-dev! :ferment.http/default)

;; Later
(app/stop!)
```

## 3) Canonical `/v1/act` request (text response intent)

Minimal protocol call routed through contracts, resolver, and capability execution.

```bash
curl -s http://127.0.0.1:12002/v1/act \
  -H 'Content-Type: application/json' \
  -d '{
    "proto": 1,
    "trace": {"id": "demo-1"},
    "task": {"intent": "text/respond"},
    "input": {"prompt": "Explain what Ferment does in two sentences."}
  }'
```

Notes:
- Optional routing flags in payload are canonical under `"routing"` as `"meta?"`, `"strict?"`, `"force?"`, and `"debug/plan?"`.
- Successful responses may include execution metadata like `models/used` and (for plan-based flow) `result.plan/run`.

## 4) Coding-oriented request with effects contract

Use this when the request should declare effect needs and completion criteria.

```bash
curl -s http://127.0.0.1:12002/v1/act \
  -H 'Content-Type: application/json' \
  -d '{
    "proto": 1,
    "trace": {"id": "demo-code-1"},
    "task": {"intent": "code/patch"},
    "input": {"prompt": "Add a function that validates UUID v4 and tests."},
    "done": {
      "must": ["schema-valid", "patch-applies"],
      "should": ["tests-pass", "no-hallucinated-apis"],
      "score-min": 0.8
    },
    "effects": {"allowed": ["fs/write", "process/run"]}
  }'
```

Notes:
- Runtime still enforces effect scope from config (`:ferment.runtime/default`).
- If scope is denied, endpoint returns `403` (`:effects/scope-denied`).

## 5) Session bridge for lifecycle and worker controls

Use `/v1/session` to manage runtime/session state.

```bash
# Inspect worker/session snapshot
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{"action":"state"}'
```

```bash
# Open (or refresh) a logical session
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{"action":"session/open","session/id":"sess-42"}'
```

```bash
# Freeze a model worker bound to session
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{"action":"worker/freeze","session/id":"sess-42","model":"meta"}'
```

```bash
# Thaw it back
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{"action":"worker/thaw","session/id":"sess-42","model":"meta"}'
```

### Session vars and intent-aware policy

`/v1/session` also supports CRUD for session variables:
- `session/get-var`, `session/get-vars`
- `session/put-var`, `session/put-vars`
- `session/del-var`, `session/del-vars`, `session/del-all-vars`

For vars operations, runtime enforces `session-vars` policy from config (`:policy/default`, `:policy/by-intent`, `:policy/by-operation`).

Additional contract controls:
- `:class/by-namespace` + `:class/policy`: classify vars by namespace and apply per-class TTL/freeze rules.
- `:request/default-bindings`: declarative map of session vars auto-injected into `/v1/act` request paths (for example `[:constraints :language]`, `[:input :system]`, `[:context :summary]`).

TTL resolution order on write:
1. explicit request option `:ttl-ms`,
2. class default (`:class/policy/* :ttl/default-ms`),
3. global default (`:ttl/default-ms`),
and then clamp by class/global max (`:ttl/max-ms`).

Freeze behavior on write/delete:
- when session is frozen, permission is checked per key class first,
- if class rule is missing, runtime falls back to global `:freeze/allow-write?` / `:freeze/allow-delete?`.

```bash
# Write a var in namespace "request" (allowed for put operation by default policy)
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{
    "action":"session/put-var",
    "session/id":"sess-42",
    "key":"request/topic",
    "value":"acid"
  }'
```

```bash
# Read the same var with an intent that allows "request" namespace reads
curl -s http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{
    "action":"session/get-var",
    "session/id":"sess-42",
    "intent":"route/decide",
    "key":"request/topic"
  }'
```

```bash
# Read the same var with an intent that does NOT allow "request" namespace reads
# Expected: HTTP 403 + :session.vars/policy-read-forbidden
curl -s -i http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{
    "action":"session/get-var",
    "session/id":"sess-42",
    "intent":"text/respond",
    "key":"request/topic"
  }'
```

Typical error body shape:

```json
{
  "ok?": false,
  "error": "session.vars/policy-read-forbidden",
  "message": "Session var access is forbidden by policy.",
  "details": {
    "mode": "read",
    "intent": "text/respond",
    "operation": "session/get-var"
  }
}
```

Write policy denial example:

```bash
# Attempt to write "request/*" under an intent that does not allow write there
# Expected: HTTP 403 + :session.vars/policy-write-forbidden
curl -s -i http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{
    "action":"session/put-var",
    "session/id":"sess-42",
    "intent":"text/respond",
    "key":"request/topic",
    "value":"blocked-write"
  }'
```

```json
{
  "ok?": false,
  "error": "session.vars/policy-write-forbidden",
  "message": "Session var access is forbidden by policy."
}
```

Delete policy denial example:

```bash
# Attempt to delete "request/*" under an intent that does not allow delete there
# Expected: HTTP 403 + :session.vars/policy-delete-forbidden
curl -s -i http://127.0.0.1:12002/v1/session \
  -H 'Content-Type: application/json' \
  -d '{
    "action":"session/del-var",
    "session/id":"sess-42",
    "intent":"text/respond",
    "key":"request/topic"
  }'
```

```json
{
  "ok?": false,
  "error": "session.vars/policy-delete-forbidden",
  "message": "Session var access is forbidden by policy."
}
```

## 6) Session principal mode (authenticate once, reuse session identity)

When HTTP auth is enabled and session-principal mode is enabled, `/v1/act` can use session principal without Basic Auth on every request.

Typical flow:
1. Call an authenticated endpoint once with Basic Auth.
2. Keep `session/id` in subsequent requests.
3. Runtime refreshes principal metadata in session according to configured TTL/refresh windows.

## 7) Admin API (`/v1/admin`)

Use admin actions for user, role, and migration operations.

```bash
curl -s http://127.0.0.1:12002/v1/admin \
  -H 'Content-Type: application/json' \
  -d '{
    "action": "admin/create-user",
    "email": "operator@example.org",
    "password": "change-me",
    "account-type": "operator"
  }'
```

```bash
curl -s http://127.0.0.1:12002/v1/admin \
  -H 'Content-Type: application/json' \
  -d '{
    "action": "admin/grant-role",
    "selector": "operator@example.org",
    "role": "role/operator"
  }'
```

```bash
curl -s http://127.0.0.1:12002/v1/admin \
  -H 'Content-Type: application/json' \
  -d '{"action":"admin/migrate-db"}'
```

## 8) Direct model runtime endpoints (HTTP to worker bridge)

Each model runtime can expose its own endpoint when `:http {:enabled? true ...}` is set in model runtime config.

Examples from current defaults:
- `/solver/responses`
- `/voice/responses`
- `/coding/responses`
- `/meta/responses`

Example call:

```bash
curl -s http://127.0.0.1:12002/meta/responses \
  -H 'Content-Type: application/json' \
  -d '{"prompt":"Return one short sentence in Polish."}'
```

## 9) Testing profiles and runtime modes

Use profile-specific test commands:

```bash
# test profile, mock LLM mode by default
bin/test

# full test suite alias
bin/test-full

# test-live profile, live mode defaults
bin/test-live
```

Current convention:
- `test` is optimized for predictable local test runs.
- `test-live` is for live-model behavior checks under controlled setup (shared small runtime for `meta`/`solver`/`coding` + separate small `voice` runtime).

## 10) Routing policy profiles (`low-latency`, `balanced`, `high-quality`)

`/v1/act` accepts routing profile selection through `routing.profile`.

```bash
curl -s http://127.0.0.1:12002/v1/act \
  -H 'Content-Type: application/json' \
  -d '{
    "proto": 1,
    "trace": {"id": "latency-1"},
    "task": {"intent": "text/respond"},
    "routing": {"profile": "low-latency"},
    "input": {"prompt": "Explain ACID briefly and give one example."}
  }'
```

Profile intent:
- `low-latency`: minimal retries/fallback.
- `balanced`: default runtime behavior.
- `high-quality`: higher retry/fallback budget and stricter quality recovery.

## 11) Debug transcript and timings for multi-model flows

Enable routing transcript diagnostics for `/v1/act`:

```bash
curl -s http://127.0.0.1:12002/v1/act \
  -H 'Content-Type: application/json' \
  -d '{
    "proto": 1,
    "trace": {"id": "debug-transcript-1"},
    "session/id": "session/debug-transcript-1",
    "task": {"intent": "text/respond"},
    "routing": {"meta?": true, "strict?": true, "force?": true, "debug/transcript?": true},
    "input": {"prompt": "Explain ACID briefly and give one example."}
  }' | jq '.result["plan/run"] | {participants, timings, transcript}'
```

Contract:
- final user output is sanitized (no `<think>` / tool markers),
- raw model artifacts stay only in transcript diagnostics.

### 11.1) Training mode (`:training`)

HTTP config supports explicit training branch:

```edn
:training {:enabled? false
           :transcript/intents [:text/respond :code/patch]}
```

Behavior:
- when training is enabled, replay recording defaults to `true` per request,
- debug transcript auto-enables only for intents listed in `:transcript/intents`,
- explicit request override still wins:
  - `:training/enabled? false` disables training consequences for that request,
  - `:routing {:debug/transcript? false}` keeps transcript disabled even in training mode.

`test-live` profile enables training by default in:
- `resources/config/common/test-live/training.edn`

## 12) Deterministic replay package and diff

Enable replay storage in HTTP config (`resources/config/common/prod/http.edn`):

```edn
:replay {:enabled? true
         :ttl-ms 86400000
         :max-size 512
         :redact-keys [:password :secret :token :authorization]}
```

Read replay package for a trace id:

```bash
curl -s "http://127.0.0.1:12002/v1/act/replay/<trace-id>" | jq
```

Compare two replays (post-mortem diff):

```bash
curl -s "http://127.0.0.1:12002/v1/act/replay/<trace-a>?against=<trace-b>" | jq '.comparison'
```

Operator shortcut:

```bash
bin/replay-diff <trace-a> <trace-b>
```

Raw JSON mode:

```bash
bin/replay-diff <trace-a> <trace-b> --raw
```

Save full JSON response to file:

```bash
bin/replay-diff <trace-a> <trace-b> --save target/replay-diff.json
```

Deterministic rerun from stored replay payload:

```bash
curl -s -X POST "http://127.0.0.1:12002/v1/act/replay/<trace-id>/rerun" \
  -H 'Content-Type: application/json' \
  -d '{}' | jq
```

Replay package includes:
- frozen request payload (`payload/prepared/resolved`),
- routing decision snapshot (`candidates`, `selected-cap/id`, rejected candidates),
- policy snapshot (`policy/snapshot-id` + payload),
- deep diagnostics (`execution-path`, telemetry `before/after/delta`),
- final response envelope and timing.

Replay comparison output includes automated policy/config diff report:
- `comparison.policy/config.same?`
- `comparison.policy/config.diff` (recursive `from/to` for changed fields)
- `comparison.policy/snapshot-id` when snapshot ids diverge.

### 12.1) Export replay to training JSONL (`training.event/v1` + LoRA rows)

Exporter reads replay records from JSON/JSONL and writes:
- canonical events (`training.event/v1`) to JSONL,
- LoRA SFT rows (`prompt`/`completion`) to JSONL.

Example (single replay response saved from `/v1/act/replay/<trace-id>`):

```bash
curl -s "http://127.0.0.1:12002/v1/act/replay/<trace-id>" > target/replay-trace.json
bin/export-training-events --in target/replay-trace.json
```

Custom outputs:

```bash
bin/export-training-events \
  --in target/replay-trace.json \
  --out-events target/training/events-v1.jsonl \
  --out-train data/train.jsonl \
  --train-task :meta-protocol
```

Input contract:
- JSON object with `:replay` branch (response shape of `/v1/act/replay/<trace-id>`), or
- raw replay entry object, or
- JSONL with one of the above per line.

`train.jsonl` includes accepted attempts by default; use `--include-failed` to include failed attempts too.

Target trainer row formats:
- `:sft-prompt-completion` (default): `{:prompt ... :completion ...}`
- `:messages`: `{:messages [{:role ... :content ...} ...]}`
- `:chatml`: alias to `:messages` row shape

Example with explicit target format:

```bash
bin/export-training-events \
  --in target/replay-trace.json \
  --target-format :messages \
  --out-train target/training/train-messages.jsonl
```

### 12.2) Build deterministic train/valid/test dataset + manifest

Build reproducible dataset artifacts from replay records or canonical `training.event/v1` JSONL:

```bash
bin/build-training-dataset \
  --in target/training/events-v1.jsonl \
  --out-dir target/training/dataset \
  --target-format :messages \
  --split-seed 20260304 \
  --train-ratio 0.8 \
  --valid-ratio 0.1 \
  --test-ratio 0.1
```

Idempotency behavior (default enabled):
- source files are fingerprinted (`size`, `mtime`, optional checksum),
- unchanged sources are skipped (`:skip/reason :idempotency/sources-unchanged`),
- already exported `training.event/id` values are not exported again,
- when only new events arrive, builder appends new rows (`:mode :incremental-append`).
- when dataset config changes (split/format/include-failed/train-task), builder enforces full rebuild (`:mode :full-rebuild`, `:mode/reason :config-changed`).

Input forms for `--in`:
- single JSON/JSONL file,
- directory with `*.json` / `*.jsonl` files (sorted by filename),
- comma-separated list of file/directory paths.

CLI idempotency options:
- `--state-file PATH` (state under `--out-dir`, default `.dataset-state.json`),
- `--no-source-checksum` (faster fingerprinting),
- `--fail-on-config-change` (abort build instead of full rebuild when config hash changes),
- `--no-idempotency` (always rebuild from input).

Generated artifacts:
- `events-v1.jsonl` (deduplicated canonical events),
- `train.jsonl`, `valid.jsonl`, `test.jsonl` (deterministic split),
- `manifest.json` with counts, hashes, filters, time window, and stable `snapshot/id`.

Quick verification:

```bash
jq '.["snapshot/id"], .counts, .hashes, .["time/window"]' target/training/dataset/manifest.json
```

### 12.3) Offline student evaluation + promotion gate

Run eval suites and promotion decision from exported training rows/events:

```bash
bin/eval-student \
  --in target/training/events-v1.jsonl \
  --out-report target/training/eval-report.json \
  --out-promotion target/training/promotion-report.json
```

Use custom thresholds (CLI overrides config):

```bash
bin/eval-student \
  --in target/training/events-v1.jsonl \
  --overall-min 0.90 \
  --protocol-min 0.95 \
  --constitution-min 0.90 \
  --regression-min 0.90 \
  --fail-on-reject
```

Use external config files (root map expected for each branch):

```bash
cat > target/training/eval-config.edn <<'EOF'
{:suites [:protocol-conformance :constitution-compliance :regression]
 :report {:include-cases? true :failed-only? false}}
EOF

cat > target/training/promotion-config.edn <<'EOF'
{:enabled? true
 :blocking? true
 :required-suites [:protocol-conformance :constitution-compliance :regression]
 :thresholds {:overall/pass-rate-min 0.85
              :suite-pass-rate-min {:protocol-conformance 0.90
                                    :constitution-compliance 0.90
                                    :regression 0.90}}}
EOF

bin/eval-student \
  --in target/training/events-v1.jsonl \
  --eval-config target/training/eval-config.edn \
  --promotion-config target/training/promotion-config.edn
```

Notes:
- accepted inputs: JSON array, JSONL, or line-delimited JSON objects,
- report includes per-suite and overall pass-rate summary with deterministic `failed/case-ids`,
- promotion report includes explicit rejection reasons and normalized threshold config,
- with `--fail-on-reject`, process exits with code `3` on gate reject (useful for CI).

## 13) Repeatable live benchmark pack

Run canonical benchmark suite against a running node:

```bash
bin/benchmark-live
```

Run low-latency preset (case pack with `routing.profile=low-latency`):

```bash
bin/benchmark-live --preset low-latency
```

Run SLA preset (interactive/user-facing path, no forced meta-strict cases):

```bash
bin/benchmark-live --preset sla
```

Run N repeated executions with aggregate report (p50/p95 across runs):

```bash
bin/benchmark-live --preset sla --runs 5
```

Optional endpoint override:

```bash
FERMENT_BENCH_URL=http://127.0.0.1:12002 bin/benchmark-live
```

Optional explicit case directory override:

```bash
bin/benchmark-live --case-dir resources/bench/act-low-latency
```

Artifacts:
- single run (`--runs 1`, default) writes to `target/benchmarks/<timestamp>/`:
  - `results.json`
  - `telemetry-before.json`
  - `telemetry-after.json`
  - `summary.json`
  - `summary.md`
- multi run (`--runs N`) writes to `target/benchmarks/<timestamp>/`:
  - per run: `run-01/`, `run-02/`, ... each with single-run artifacts
  - aggregate: top-level `summary.json` and `summary.md` with cross-run p50/p95, pass-rate, and aggregate `no truncated ending` gate

Built-in gates:
- `text/respond interactive/default p95 <= 10s` (hard gate)
- `text/respond strict/orchestration p95 <= 40s` (informational split gate)
- `must-failed-rate SLA <= 0.20` (excludes expected recovery case `c3_solver_handoff`)
- `must-failed-rate global <= 0.20` (informational, from telemetry snapshot)
- `route/fail-closed` only for strict requests and with routing details.
- `text/respond` outputs must not end in truncated sentence fragments (hard gate).
- `c3_solver_handoff` must include solver participation (`models/used` contains `llm/solver`) in `default` preset.
- `c4->c5 context recall` must keep `MariaDB` in follow-up answer (`c5_context_turn2`; benchmark case has `routing.meta? = false` to isolate memory behavior from meta-decider).

Per-case result payload (`results.json`) also includes normalized workflow timing fields:
- `call_timings` (with `latency_ms`),
- `call_latency_ms_total`,
- `call_latency_ms_max`,
- `route_decide_latency_ms` (latency of decider invocation itself),
- `route_decider_latency_ms` (same value as explicit alias for readability),
- `route_phase_latency_ms` (full meta-routing phase in HTTP bridge),
so ad-hoc jq analysis does not depend on hyphenated JSON keys.

## 14) Async `/v1/act` queue flow (operator mode)

Use this flow when request execution should be accepted quickly and completed out-of-band.

Submit async job:

```bash
curl -s http://127.0.0.1:12002/v1/act \
  -H 'Content-Type: application/json' \
  -d '{
    "proto": 1,
    "trace": {"id": "async-1"},
    "response/type": "accepted",
    "task": {"intent": "text/respond"},
    "queue": {"class": "interactive", "deadline-ms": 20000},
    "input": {"prompt": "Explain ACID briefly and give one example."}
  }' | jq
```

Expected response: `202` with canonical accepted payload (`job/id`, `job/status=queued`, timestamps, queue class).

Poll job status:

```bash
JOB_ID="job/123"
curl -s "http://127.0.0.1:12002/v1/act/jobs/${JOB_ID}" | jq
```

Cancel job:

```bash
JOB_ID="job/123"
curl -s -X POST "http://127.0.0.1:12002/v1/act/jobs/${JOB_ID}/cancel" \
  -H 'Content-Type: application/json' \
  -d '{"cancel/reason":"operator"}' | jq
```

Queue observability:

```bash
curl -s http://127.0.0.1:12002/diag/telemetry | jq '.telemetry.queue'
```

Key counters:
- `jobs/submitted`
- `jobs/started`
- `jobs/completed`
- `jobs/failed`
- `jobs/canceled`
- `jobs/expired`

Queue config source of truth:
- base defaults: `resources/config/common/prod/runtime.edn` (`:ferment.runtime/default :queue`)
- dev overlay: `resources/config/common/dev/runtime.edn`
- test-live overlay: `resources/config/common/test-live/runtime.edn`

Important knobs:
- `enabled?`, `max-size`, `workers`, `priority-order`
- `default-timeout-ms`, `default-deadline-ms`, `max-deadline-ms`
- `retry.max-attempts`, `retry.base-backoff-ms`, `retry.jitter-ms`
