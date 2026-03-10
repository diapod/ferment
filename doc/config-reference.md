# Configuration Reference

This document describes Ferment runtime configuration.

Conventions used below:

- `k?` means optional key.
- `kw` means keyword.
- `ms` means integer milliseconds.
- `ref` means Integrant/reader reference (for example `#ref …`).

## Loading and Layering

### `:ferment.app/properties`

Purpose: application identity and active profile.
Main file: `resources/config/common/prod/config.edn`.

Shape (EDN):
```edn
{:name        string
 :title       string
 :profile     kw
 :author      string
 :description string
 :version     string
 :license     string}
```

Typical overlays:
- `resources/config/common/dev/profile.edn`
- `resources/config/common/test/profile.edn`
- `resources/config/common/test-live/profile.edn`
- `resources/config/common/admin/profile.edn`

### `:ferment.system/properties`

Purpose: system-level app metadata binding.
Main file: `resources/config/common/prod/config.edn`.

Shape (EDN):
```edn
ref ; usually #ref :ferment.app/properties
```

### `:ferment.system/timezone`

Purpose: canonical timezone used by runtime/logging/db helpers.
Main file: `resources/config/common/prod/config.edn`.

Shape (EDN):
```edn
kw ; for example :UTC
```

## Runtime Core

### `:ferment.runtime/default`

Purpose: top-level runtime wiring and execution policy.
Main file: `resources/config/common/prod/runtime.edn`.

Shape (EDN):
```edn
{:models                         ref
 :router                         ref
 :resolver                       ref
 :protocol                       ref
 :roles                          ref
 :session                        ref
 :ferment.model.session/enabled? boolean
 :effects                        {:fs/write   {:enabled?       boolean
                                               :root           string
                                               :allow          [string …]}
                                 :process/run {:enabled?       boolean
                                               :root           string
                                               :allow-cwd      [string …]
                                               :allow-commands [string …]}
                                 :net/http    {:enabled?       boolean
                                               :allow-schemes  #{kw …}
                                               :allow-hosts    [string …]
                                               :allow-ports    [int …]}}
 :queue {:enabled?            boolean
         :max-size            int
         :classes             [kw …]
         :priority-order      [kw …]
         :default-class       kw
         :workers             int
         :poll-interval-ms    ms
         :default-timeout-ms  ms
         :default-deadline-ms ms
         :max-deadline-ms     ms
         :retry               {:max-attempts    int
                               :base-backoff-ms ms
                               :jitter-ms       ms}}
 :execution-graph {:enabled?   boolean
                   :store/type kw
                   :store/path string
                   :max-events int}
 :tenancy {:enabled?       boolean
           :default-tenant kw
           :default        {:limits {:requests-per-minute int?
                              :max-concurrent-requests int?
                              :max-tokens-per-request  int?
                              :max-timeout-ms          ms?
                              :daily-max-billed-tokens int?}
                     :routing/defaults map?}
           :tenants           {kw map …}
           :principal->tenant {string kw …}}
 :oplog ref}
```

Overlay examples:
- `resources/config/common/dev/runtime.edn`
- `resources/config/common/test-live/runtime.edn`

### `:ferment.core/default`

Purpose: core service composition.
Main file: `resources/config/common/prod/core.edn`.

Shape (EDN):
```edn
{:runtime  ref
 :resolver ref
 :protocol ref
 :session  ref}
```

## Routing and Resolution

### `:ferment.router/default`

Purpose: routing behavior, policy profiles, and defaults.
Main file: `resources/config/common/prod/router.edn`.

Shape (EDN):
```edn
{:routing                ref
 :artifact/version       kw
 :versions               {kw map …}
 :rollout                {:active kw
                          :canary {:enabled? boolean
                                   :version  kw
                                   :percent  int}
                          :shadow {:enabled? boolean
                                   :version  kw
                                   :percent  int}}
 :intent->policy-profile {kw kw …}
 :policy-profiles
 {kw {:default {:retry {:same-cap-max int
                        :fallback-max int}
                :switch-on #{kw …}
                :done      {:score-min number}}
      :intents {kw {:retry {:same-cap-max int
                            :fallback-max int}
                    :done {:score-min number}}}
      :limits {:call-tree/max-calls         int
               :call-tree/max-fallback-hops int}}
  …}
 :profiles {kw {:meta?          boolean
                :strict?        boolean
                :force?         boolean
                :on-error       kw
                :policy/profile kw} …}
 :defaults {:meta?          boolean
            :strict?        boolean
            :force?         boolean
            :on-error       kw
            :policy/profile kw}
 :policy kw}
```

### `:ferment.resolver/default`

Purpose: capability registry binding for resolver.
Main file: `resources/config/common/prod/resolver.edn`.

Shape (EDN):
```edn
{:caps     [ref …]
 :protocol ref}
```

### `:ferment.caps/routing`

Purpose: declarative intent/cap/model/role mapping and route-level policy defaults.
Main file: `resources/config/common/prod/capabilities.edn`.

Shape (EDN):
```edn
{:intent->cap               {kw kw …}
 :intent->candidates        {kw [kw …] …}
 :intent->default-model-key {kw kw …}
 :cap->role                 {kw kw …}
 :intent->default-role      {kw kw …}
 :switch-on                 #{kw …}
 :retry                     {:same-cap-max int
                             :fallback-max int}
 :gateway {:strategy         kw
           :intent->strategy {kw kw …}
           :transport-order  [kw …]
           :intent->transport-order {kw [kw …] …}
           :ema-alpha        number
           :hedging          {:enabled?         boolean
                              :intent->enabled? {kw boolean …}
                              :max-probes       int
                              :delay-ms         ms}
           :circuit-breaker  {:enabled?         boolean
                              :min-samples      int
                              :error-rate-open  number
                              :cooldown-ms      ms}}
 :policy kw}
```

## Protocol and Quality

### `:ferment.protocol/default`

Purpose: protocol envelope, prompt layers, intent contracts, quality checks, retry/fallback policy.
Main file: `resources/config/common/prod/protocol.edn`.

Shape (EDN):
```edn
{:proto/version          int
 :meta-language          kw
 :transport/content-type string
 :envelope/request       {:required [kw …]
                          :optional [kw …]}
 :envelope/response {:required-one-of [kw …]}
 :prompts           {:default [string …]
                      :roles   {kw [string …] …}
                      :intents {kw [string …] …}}
 :versions          {kw {:prompts? {:default [string …]
                                    :roles   {kw [string …] …}
                                    :intents {kw [string …] …}}
                         :intents? {kw map …}
                         :policy/default? map
                         :policy/intents? {kw map …}}
                     …}
 :rollout           {:active kw
                     :canary {:enabled? boolean
                              :version  kw
                              :percent  int}
                     :shadow {:enabled? boolean
                              :version  kw
                              :percent  int}}
 :artifact/version  kw
 :intents
 {kw {:in-schema        kw
      :out-schema       kw
      :constraints      {:max-chars      int}
      :budget           {:max-tokens     int
                         :max-roundtrips int
                         :temperature    number}
      :system?          [string …]
      :result/contract? map} …}
 :policy/checks  {kw kw …}
 :policy/default {:done {:must      #{kw …}
                         :should?   #{kw …}
                         :score-min number}
                  :checks      [kw …]
                  :checks/hard [kw …]
                  :checks/soft [kw …]
                  :judge       {:enabled?     boolean
                                :intent       kw
                                :cap/id       kw
                                :role         kw
                                :max-attempts int
                                :score-path   [kw …]}
                  :retry       {:max-attempts int
                                :same-cap-max int
                                :fallback-max int}
                  :switch-on   #{kw …}
                  :fallback    [kw …]}
 :policy/intents      {kw map …}
 :roles               {kw {:allowed [kw …]} …}
 :constraints/default map
 :budget/default      map
 :effects/default     map
 :error/catalog       {kw {:retryable? boolean} …}
 :result/types        [kw …]
 :retry/max-attempts  int}
```

## Capabilities and Models

### `:ferment.caps.registry/*`

Purpose: single capability definition.
Main file: `resources/config/common/prod/capabilities.edn`.

Shape (EDN):
```edn
{:cap/id              kw
 :cap/kind            kw
 :cap/version         string
 :cap/cost            {:latency-ms ms}
 :cap/limits          {:timeout-ms ms}
 :transport/type      kw
 :transport/auth      kw
 :transport/timeout-ms ms?
 :transport/retry     {:max int
                       :backoff-ms ms}
 :cap/tags            #{kw …}
 :cap/intents         #{kw …}
 :cap/can-produce     #{kw …}
 :cap/effects-allowed #{kw …}
 :dispatch/role       kw
 :dispatch/model-key  kw
 :io/in-schema        kw
 :io/out-schema       kw
 :dispatch/tag        kw}
```

### `:ferment.caps/registry`

Purpose: aggregate capability list.
Main file: `resources/config/common/prod/capabilities.edn`.

Shape (EDN):
```edn
[ref …]
```

### `:ferment.caps/profiles`

Purpose: profile-specific capability mode knobs.
Main file: `resources/config/common/prod/capabilities.edn`.

Shape (EDN):
```edn
{kw {:llm/mode kw} …}
```

### `:ferment.model.defaults/profile`

Purpose: default model profile selector.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
string
```

### `:ferment.model.defaults/bot-session`

Purpose: default runtime worker session metadata.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{:sid string}
```

### `:ferment.model.defaults/runtime`

Purpose: default runtime process/invoke options inherited by role runtimes.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{:session     ref
 :enabled?    boolean
 :inherit-io? boolean
 :prompt-via  kw
 :env         {string string …}}
```

### `:ferment.model.id/*`

Purpose: role model identity map.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{:profile     string|ref
 :type        kw
 :id/default  string
 :id/mini     string
 :id/fallback string}
```

### `:ferment.model.runtime/*`

Purpose: role runtime worker and invoke transport config.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{:defaults    ref
 :command     [string|ref …]
 :invoke/http {:base-url     string|ref
               :endpoint     string|ref
               :model        string|ref
               :headers?     {string string|ref …}
               :timeout-ms?  ms
               :retries?     int
               :retry-ms?    ms
               :temperature? number
               :top-p?       number
               :max-tokens?  int}
 :http        {:enabled? boolean
               :endpoint string}
 :name        string}
```

### `:ferment.model/*`

Purpose: role binding to id/runtime branches.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{:id      ref
 :runtime ref}
```

### `:ferment/models`

Purpose: aggregate model map used by runtime/http.
Main file: `resources/config/common/prod/models.edn`.

Shape (EDN):
```edn
{kw ref …}
```

## HTTP and ACT Pipeline

### `:ferment.http/default`

Purpose: HTTP server and `/v1/act` operational branches.
Main file: `resources/config/common/prod/http.edn`.

Shape (EDN):
```edn
{:host           string
 :port           int
 :act/middleware [ref …]
 :response-cache {:enabled? boolean
                  :ttl-ms   ms
                  :max-size int}
 :replay         {:enabled? boolean
                  :ttl-ms      ms
                  :max-size    int
                  :redact-keys [kw …]}
 :training       {:enabled? boolean
                  :transcript/intents [kw …]
                  :judge              {:mode             kw
                                       :constitution/ref string|nil
                                       :rules            [kw …]}
                  :redaction {:enabled?      boolean
                              :placeholder   string
                              :deny/keys     [kw …]
                              :deny/paths    [vector …]
                              :deny/patterns [string …]}
                  :collector {:enabled?            boolean
                              :store/type          kw
                              :store/path          string
                              :flush-policy        kw
                              :max-file-size-bytes int}
                  :dataset {:split {:ratios {:train number
                                             :valid number
                                             :test  number}
                                    :seed int}
                            :include-failed? boolean
                            :idempotency     {:enabled?               boolean
                                              :state-file             string
                                              :source-checksum?       boolean
                                              :fail-on-config-change? boolean}}
            :export {:target-format kw
                     :out-dir       string
                     :out-events    string
                     :out-train     string
                     :sanity-check {:enabled? boolean
                                    :row/fn symbol|nil}}
            :eval {:enabled?   boolean
                   :suites     [kw …]
                   :report     {:include-cases? boolean
                                :failed-only?   boolean}
                   :thresholds map}
            :promotion {:enabled?        boolean
                        :blocking?       boolean
                        :required-suites [kw …]
                        :thresholds      map}}
 :models ref
 :auth   {:enabled?          boolean
          :realm             string
          :session-principal {:enabled?   boolean
                              :operations #{kw …}
                              :ttl-ms     ms
                              :refresh-ms ms}}}
```

### `:ferment.http.act.middleware/*`

Purpose: middleware module entries used to compile ACT chain.
Main file: `resources/config/common/prod/http.edn`.

Shape (EDN):
```edn
{} ; module options map (currently empty for built-ins)
```

## Session and Memory

### `:ferment.session.store/default`

Purpose: session storage backend and session-vars contract.
Main file: `resources/config/common/prod/session.edn`.

Shape (EDN):
```edn
{:backend        kw
 :db             ref
 :sessions-table kw
 :vars-table     kw
 :session-vars/contract
 {:keys/require-qualified?  boolean
  :keys/allowed-namespaces  #{string …}
  :ttl/default-ms           ms
  :ttl/max-ms               ms
  :freeze/allow-write?      boolean
  :freeze/allow-delete?     boolean
  :limits/max-vars          int
  :limits/max-key-chars     int
  :limits/max-value-bytes   int
  :class/default            kw
  :class/by-namespace       {string kw …}
  :class/policy             {kw {:ttl/default-ms       ms
                                 :ttl/max-ms           ms
                                 :freeze/allow-write?  boolean
                                 :freeze/allow-delete? boolean} …}
  :request/default-bindings {kw {:target [kw …]
                                 :coerce kw} …}
  :memory/policy {:enabled?                 boolean
                  :read/default?            boolean
                  :read/by-intent           {kw boolean …}
                  :write/default?           boolean
                  :write/by-intent          {kw boolean …}
                  :write/key                kw
                  :write/max-chars          int
                  :principal/isolation?     boolean
                  :principal/key            kw
                  :history/enabled?         boolean
                  :history/key              kw
                  :history/max-items        int
                  :compaction/trigger-chars int
                  :compaction/target-chars  int
                  :compaction/mode          kw}
  :policy/default {:read-namespaces   #{string …}
                   :write-namespaces  #{string …}
                   :delete-namespaces #{string …}}
  :policy/by-intent    {kw map …}
  :policy/by-operation {kw map …}}
 :max-sessions int}
```

### `:ferment.session.context/default`

Purpose: context window/summary sizing.
Main file: `resources/config/common/prod/session.edn`.

Shape (EDN):
```edn
{:context/version      int
 :window/turns         int
 :window/max-chars     int
 :summary/target-chars int}
```

### `:ferment.session.manager/default`

Purpose: manager policy and references.
Main file: `resources/config/common/prod/session.edn`.

Shape (EDN):
```edn
{:store            ref
 :context          ref
 :max-hot-sessions int
 :idle-ttl-ms      ms}
```

### `:ferment.session/default`

Purpose: public session branch for runtime/core/http.
Main file: `resources/config/common/prod/session.edn`.

Shape (EDN):
```edn
{:manager  ref
 :store    ref
 :context  ref}
```

## Auth and Roles

### `:ferment.auth.pwd/suite.*`

Purpose: password algorithm suites.
Main file: `resources/config/common/prod/auth.edn`.

Shape (EDN):
```edn
[{:name          kw
  :handler       symbol
  :salt-length   int
  :parallel?     int
  :iterations?   int
  :salt-charset? string}
 …]
```

### `:ferment.auth.pwd/settings.*`

Purpose: suite-level auth timing and suite binding.
Main file: `resources/config/common/prod/auth.edn`.

Shape (EDN):
```edn
{:wait        number
 :wait-random [number number]
 :wait-nouser number
 :suite       ref}
```

### `:ferment.auth/strong` and `:ferment.auth/simple`

Purpose: account-type auth policies.
Main file: `resources/config/common/prod/auth.edn`.

Shape (EDN):
```edn
{:passwords                 ref
 :db?                       ref
 :account-types             [kw …]
 :locking/max-attempts      int
 :locking/lock-wait         [int kw]
 :locking/fail-expires      [int kw]
 :registration/expires      [int kw]
 :confirmation/expires      [int kw]
 :confirmation/max-attempts int}
```

### `:ferment.auth/setup`

Purpose: top-level auth setup.
Main file: `resources/config/common/prod/auth.edn`.

Shape (EDN):
```edn
{:db           ref
 :default-type kw
 :types        [ref …]}
```

### `:ferment.roles/default`

Purpose: operation/effect authorization matrix.
Main file: `resources/config/common/prod/roles.edn`.

Shape (EDN):
```edn
{:enabled?            boolean
 :authorize-default?  boolean
 :global-context      kw
 :anonymous-role      kw
 :logged-in-role      kw
 :account-type->roles {kw #{kw …} …}
 :operations          {kw {:any        #{kw …}
                           :all?       #{kw …}
                           :forbidden? #{kw …}} …}
 :effects {kw {:any        #{kw …}
               :all?       #{kw …}
               :forbidden? #{kw …}} …}}
```

## Database and Migration

### `:ferment.db/main.props`

Purpose: DB connection properties.
Main file: `resources/config/common/prod/databases.edn`.

Shape (EDN):
```edn
{:dbtype               string|ref
 :dbname               string|ref
 :logger               ref
 :app-timezone         kw|ref
 :maximumPoolSize      int
 :connectionTestQuery  string
 :dataSourceProperties map}
```

### `:ferment.db/main`

Purpose: DB pool lifecycle branch.
Main file: `resources/config/common/prod/databases.edn`.

Shape (EDN):
```edn
{:properties  ref
 :caches      ref
 :initializer symbol
 :finalizer   symbol
 :suspender   symbol
 :resumer     symbol}
```

### `:ferment.db/caches`

Purpose: cache definitions for DB-backed reads.
Main file: `resources/config/common/prod/databases.edn`.

Shape (EDN):
```edn
{kw {:size? int
     :ttl?  [int kw]} …}
```

### `:ferment.db/main.migrator`

Purpose: migrator branch (admin profile).
Main file: `resources/config/common/admin/databases.edn`.

Shape (EDN):
```edn
{:properties     ref
 :initializer    symbol
 :loader         symbol
 :migrations-dir string
 :create-extra   string}
```

### `:ferment.db/migrators`

Purpose: migrator list for admin commands.
Main file: `resources/config/common/admin/databases.edn`.

Shape (EDN):
```edn
[ref …]
```

## Observability and Network Helpers

### `:ferment.logging/unilog`

Purpose: main logging configuration.
Main file: `resources/config/common/prod/logging.edn`.

Shape (EDN):
```edn
{:system              ref
 :prev-config         map
 :level               kw
 :console             boolean
 :appenders           [map …]
 :overrides           {string kw …}
 :context-transformer map?}
```

### `:ferment.oplog.*`

Purpose: buffered DB operation log branches.
Main file: `resources/config/common/prod/logging.edn`.

Shape (EDN):
```edn
{:db           ref
 :table        kw
 :timeout      [int kw]
 :buffered-max int}
```

### `:ferment.logging/oplog`

Purpose: aggregate oplog branch.
Main file: `resources/config/common/prod/logging.edn`.

Shape (EDN):
```edn
{:auth  ref
 :act   ref}
```

### `:ferment.middleware.remote-ip/reserved`

Purpose: reserved CIDR ranges used by remote IP middleware.
Main file: `resources/config/common/prod/reserved-ip.edn`.

Shape (EDN):
```edn
[string …] ; CIDR strings
```

## Profile Overlays

### `dev` (`resources/config/common/dev/*`)

Purpose: local development overrides.

Typical keys changed:
- `:ferment.app/properties/:profile`
- `:ferment.router/default/:defaults`
- `:ferment.protocol/default/:policy/*`
- `:ferment.runtime/default/:queue`
- logging verbosity/appenders

### `test` (`resources/config/common/test/*`)

Purpose: deterministic test defaults.

Typical keys changed:
- `:ferment.app/properties/:profile`
- `:ferment.model.runtime/*/:enabled?` (disabled)

### `test-live` (`resources/config/common/test-live/*`)

Purpose: lightweight live-smoke and benchmark tuning.

Typical keys changed:
- `:ferment.app/properties/:profile`
- `:ferment.router/default/:defaults`
- `:ferment.protocol/default/:intents` and `:policy/intents`
- `:ferment.runtime/default/:queue|:execution-graph|:tenancy`
- `:ferment.model.id/*`, `:ferment.model.runtime/*`
- `:ferment.http/default/:training`

### `admin` (`resources/config/common/admin/*`)

Purpose: migrations/admin flows.

Typical keys changed:
- `:ferment.app/properties/:profile`
- `:ferment.db/main.migrator`, `:ferment.db/migrators`

## Local Overrides

### `resources/config/local/<profile>/*`

Purpose: machine-local config and secrets.

Common examples:
- `db.env`
- `main-db.edn`
- `locations.env`

Merge rule (practical):
- base common/prod -> local/prod -> common/<profile> -> local/<profile>
- later layers override earlier values for the same keys.
