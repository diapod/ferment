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
- `judge-pass-rate`
- `cache-hit-rate`
- `failure-taxonomy` (`by-type` + `by-domain`)

Lifecycle observability is available under `telemetry.lifecycle` (`total`, `errors`, per-component transitions, recent events window).

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
