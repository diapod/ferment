# Configuration Reference

This document describes the runtime configuration used by Ferment.

## Loading Model and Layering

### `:ferment.app/properties`

- Purpose: application identity and active profile.
- Main location: `resources/config/common/prod/config.edn`.

- Typical overlays:
  - `resources/config/common/dev/profile.edn`
  - `resources/config/common/test/profile.edn`
  - `resources/config/common/test-live/profile.edn`
  - `resources/config/common/admin/profile.edn`

- Important subkeys:
  - `:name`, `:title`, `:version`, `:license`
  - `:profile` (`:prod`, `:dev`, `:test`, `:test-live`, `:admin`)

### `:ferment.system/properties` and `:ferment.system/timezone`

- Purpose: bootstrap-level system metadata and timezone.
- Main location: `resources/config/common/prod/config.edn`.

- Notes:
  - `:ferment.system/properties` usually points to `#ref :ferment.app/properties`.
  - `:ferment.system/timezone` is reused by other modules.

## Runtime Core and Execution

### `:ferment.runtime/default`

- Purpose: top-level runtime wiring for orchestration, policies, side effects, queue, and optional execution graph.
- Main location: `resources/config/common/prod/runtime.edn`.

- Important subkeys:
  - Wiring: `:models`, `:router`, `:resolver`, `:protocol`, `:roles`, `:session`, `:oplog`
  - Effects policy: `:effects` (`:fs/write`, `:process/run`, `:net/http`)
  - Queue policy: `:queue` (`:enabled?`, workers, timeouts, retry)
  - Trace/log of execution graph: `:execution-graph`
  - Multi-tenant controls: `:tenancy`

- Overlay examples:
  - `resources/config/common/dev/runtime.edn`
  - `resources/config/common/test-live/runtime.edn`

### `:ferment.core/default`

- Purpose: core service composition for request handling.
- Main location: `resources/config/common/prod/core.edn`.

- Important subkeys:
  - `:runtime`, `:resolver`, `:protocol`, `:session`

## Routing and Capability Resolution

### `:ferment.router/default`

- Purpose: request routing defaults, mode profiles, retry/fallback policy profiles.
- Main location: `resources/config/common/prod/router.edn`.

- Important subkeys:
  - `:routing` (reference to capability routing map)
  - `:profiles` (request-level behavior presets)
  - `:defaults` (default request routing options)
  - `:policy-profiles` (`:low-latency`, `:balanced`, `:high-quality`)
  - `:intent->policy-profile`
  - `:policy` (routing strategy id)

### `:ferment.resolver/default`

- Purpose: capability registry binding used by runtime resolver.
- Main location: `resources/config/common/prod/resolver.edn`.

- Important subkeys:
  - `:caps` (resolved list of capabilities)
  - `:protocol`

### `:ferment.caps/routing`

- Purpose: declarative routing map for intent/cap/model/role defaults.
- Main location: `resources/config/common/prod/capabilities.edn`.

- Important subkeys:
  - `:intent->cap`
  - `:intent->default-model-key`
  - `:cap->role`
  - `:intent->default-role`
  - `:switch-on`, `:retry`
  - `:gateway` (strategy, hedging, circuit-breaker)
  - `:policy`

## Protocol, Contracts, and Quality

### `:ferment.protocol/default`

- Purpose: protocol envelope, per-intent schema constraints, prompt layers, and quality/retry policies.
- Main location: `resources/config/common/prod/protocol.edn`.

- Important subkeys:
  - Envelope: `:proto/version`, `:envelope/request`, `:envelope/response`
  - Prompt stack: `:prompts/:default`, `:prompts/:roles`, `:prompts/:intents`
  - Intent contracts: `:intents/*/:in-schema`, `:out-schema`, `:constraints`, `:budget`
  - Quality checks: `:policy/checks`
  - Policy defaults: `:policy/default`
  - Per-intent overrides: `:policy/intents`
  - Error semantics: `:error/catalog`

## Capability Registry and Models

### `:ferment.caps.registry/*` and `:ferment.caps/registry`

- Purpose: capability definitions and optional aggregate registry list.
- Main location: `resources/config/common/prod/capabilities.edn`.

- Capability shape (main fields):
  - `:cap/id`, `:cap/kind`, `:cap/intents`, `:cap/can-produce`
  - `:dispatch/role`, `:dispatch/model-key`, `:dispatch/tag`
  - `:io/in-schema`, `:io/out-schema`
  - `:cap/effects-allowed`, `:cap/limits`, `:cap/cost`

### `:ferment.model.defaults/*`

- Purpose: shared model defaults (profile, runtime defaults, runtime session defaults).
- Main location: `resources/config/common/prod/models.edn`.

- Important subkeys:
  - `:ferment.model.defaults/profile`
  - `:ferment.model.defaults/runtime`
  - `:ferment.model.defaults/bot-session`

### `:ferment.model.id/*`

- Purpose: logical model identity per role with profile-specific ids.
- Main location: `resources/config/common/prod/models.edn`.

- Important subkeys:
  - `:id/default`, `:id/mini`, `:id/fallback`
  - `:type`, `:profile`

### `:ferment.model.runtime/*`

- Purpose: runtime worker/invoke transport configuration per role.
- Main location: `resources/config/common/prod/models.edn`.

- Important subkeys:
  - Process startup: `:command`, `:defaults`
  - HTTP invoke: `:invoke/http`, `:http`
  - Metadata: `:name`

### `:ferment.model/*` and `:ferment/models`

- Purpose: model bindings per role and aggregated model map.
- Main location: `resources/config/common/prod/models.edn`.

- Important subkeys:
  - Role branches (`:ferment.model/solver`, `:voice`, `:coding`, `:meta`)
  - Aggregate map (`:ferment/models`)

## HTTP API and ACT Pipeline

### `:ferment.http/default`

- Purpose: HTTP server runtime and `/v1/act` operational branches.
- Main location: `resources/config/common/prod/http.edn`.

- Important subkeys:
  - Server: `:host`, `:port`
  - ACT pipeline: `:act/middleware`
  - Optional response cache: `:response-cache`
  - Optional replay buffer: `:replay`
  - Training branch: `:training` (collector/dataset/export/eval/promotion)
  - Model registry ref: `:models`
  - Endpoint auth branch: `:auth`

### `:ferment.http.act.middleware/*`

- Purpose: middleware module configuration entries for ACT chain compilation.
- Main location: `resources/config/common/prod/http.edn`.

- Default sequence:
  - `prepare`
  - `route`
  - `execute`
  - `finalize`

## Session and Working Memory

### `:ferment.session.store/default`

- Purpose: session persistence and session-vars contract/policy.
- Main location: `resources/config/common/prod/session.edn`.

- Important subkeys:
  - Backend: `:backend`, `:db`, table names
  - Contract core: `:session-vars/contract`
  - Key namespace policy: `:keys/allowed-namespaces`
  - Limits and TTL: `:limits/*`, `:ttl/*`
  - Class policy: `:class/by-namespace`, `:class/policy`
  - Request auto-bindings: `:request/default-bindings`
  - Memory behavior: `:memory/policy`
  - Access policy overlays:
    - `:policy/default`
    - `:policy/by-intent`
    - `:policy/by-operation`

### `:ferment.session.context/default`

- Purpose: context-window shaping and summary sizing.
- Main location: `resources/config/common/prod/session.edn`.

- Important subkeys:
  - `:window/turns`, `:window/max-chars`
  - `:summary/target-chars`

### `:ferment.session.manager/default` and `:ferment.session/default`

- Purpose: manager wiring and public session branch.
- Main location: `resources/config/common/prod/session.edn`.

- Important subkeys:
  - manager: `:store`, `:context`, `:max-hot-sessions`, `:idle-ttl-ms`
  - public branch: `:manager`, `:store`, `:context`

## Auth and Role Policy

### `:ferment.auth/*`

- Purpose: password suite/settings and auth setup.
- Main location: `resources/config/common/prod/auth.edn`.

- Main branches:
  - `:ferment.auth.pwd/suite.*`
  - `:ferment.auth.pwd/settings.*`
  - `:ferment.auth/strong`, `:ferment.auth/simple`
  - `:ferment.auth/setup`

### `:ferment.roles/default`

- Purpose: operation/effect authorization policy.
- Main location: `resources/config/common/prod/roles.edn`.

- Important subkeys:
  - `:enabled?`, `:authorize-default?`
  - role mapping: `:account-type->roles`
  - operation policy: `:operations`
  - effect policy: `:effects`

## Database and Migrations

### `:ferment.db/main.props` and `:ferment.db/main`

- Purpose: DB connection pool and lifecycle branch.
- Main location: `resources/config/common/prod/databases.edn`.

- Important subkeys:
  - JDBC properties and pool tuning
  - lifecycle fns: `:initializer`, `:finalizer`, `:suspender`, `:resumer`
  - cache branch reference: `:caches`

### `:ferment.db/caches`

- Purpose: read cache policy for selected DB-backed lookups.
- Main location: `resources/config/common/prod/databases.edn`.

### `:ferment.db/main.migrator` and `:ferment.db/migrators`

- Purpose: migration runtime branch.
- Main location: `resources/config/common/admin/databases.edn`.

- Notes:
  - loaded in admin profile overlays
  - used by `bin/migrate-db` and `bin/rollback-db`

## Observability and Network Helpers

### `:ferment.logging/unilog`

- Purpose: main logging runtime config (appenders, levels, overrides, transformers).
- Main location: `resources/config/common/prod/logging.edn`.

### `:ferment.oplog.*` and `:ferment.logging/oplog`

- Purpose: buffered DB-backed operation logs and aggregator branch.
- Main location: `resources/config/common/prod/logging.edn`.

### `:ferment.middleware.remote-ip/reserved`

- Purpose: reserved CIDR list used by remote IP normalization middleware.
- Main location: `resources/config/common/prod/reserved-ip.edn`.

## Profile Overlays (Practical Map)

### `dev` overlay (`resources/config/common/dev/*`)

- Main usage: local development defaults.

- Typical overrides:
  - `:ferment.app/properties/:profile`
  - router defaults and strictness profile
  - protocol quality knobs
  - queue sizing
  - log verbosity/appenders
  - optional model profile default

### `test` overlay (`resources/config/common/test/*`)

- Main usage: deterministic test mode.

- Typical overrides:
  - profile
  - model runtimes disabled (`:enabled? false`)

### `test-live` overlay (`resources/config/common/test-live/*`)

- Main usage: lightweight live smoke and benchmark profile.

- Typical overrides:
  - profile
  - router defaults (`:low-latency`)
  - protocol budgets/checks
  - runtime queue and execution graph
  - model runtime layout for live smoke
  - optional training collector/export/eval branch

### `admin` overlay (`resources/config/common/admin/*`)

- Main usage: migration/admin flows.

- Typical overrides:
  - profile
  - migrator branches

## Local Overrides

### `resources/config/local/<profile>/*`

- Purpose: machine-local values, especially secrets and host-specific DB settings.

- Common files:
  - `db.env`
  - `main-db.edn`
  - profile-local `locations.env`

- Merge principle: profile overlays win over base prod where keys collide.

## Quick Navigation by Task

### I want to tune routing behavior

- Start with:
  - `resources/config/common/prod/router.edn`
  - `resources/config/common/prod/capabilities.edn`
  - `resources/config/common/prod/protocol.edn`

### I want to tune model selection/runtime

- Start with:
  - `resources/config/common/prod/models.edn`
  - profile overlays in `resources/config/common/<profile>/models.edn`

### I want to tune quality checks/retries

- Start with:
  - `resources/config/common/prod/protocol.edn`
  - `resources/config/common/prod/router.edn`

### I want to tune session memory behavior

- Start with:
  - `resources/config/common/prod/session.edn`

### I want to tune HTTP behavior and training export

- Start with:
  - `resources/config/common/prod/http.edn`
  - `resources/config/common/test-live/training.edn`
