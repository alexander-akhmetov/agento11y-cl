# agento11y-cl

Common Lisp SDK for [Grafana Agent Observability](https://github.com/grafana/agento11y).

It captures LLM generations, tool executions, and embeddings from your application and exports them as structured telemetry — generation payloads over HTTP and traces via OTLP.

## Features

- **Generation recording** — capture model calls with messages, token usage, tool definitions, and timing
- **Tool execution tracing** — record tool/function calls as child spans linked to parent generations
- **Embedding tracing** — track embedding API calls with input counts and token usage
- **Ad-hoc spans** — wrap arbitrary code blocks in OTel spans via `with-span`
- **Conversation ratings** — submit user feedback ratings to the API
- **Offline experiments** — create eval runs, record tagged generations, export scores, and loop a dataset through a target and scorers
- **Built-in evaluators** — grade output in-process with an LLM judge or a regex judge, and record the grader's own generation alongside the score
- **Trial artifacts** — attach files, text, or raw bytes to a trial
- **Cloud trial evaluation** — queue a tenant-stored evaluator against a trial and wait for its verdict (experimental)
- **Collection datasets** — read collections and conversations to build experiment datasets from saved conversations
- **Synchronous hook evaluation** — opt-in preflight/postflight guard checks against the hooks API with allow/deny semantics, fail-open transport handling, and `transformed_input` rewrite passthrough
- **Message normalization** — convert raw Anthropic and OpenAI API responses into SDK types
- **Background export** — batched, async HTTP export with exponential backoff retry
- **Content capture modes** — `:full`, `:no-tool-content`, `:metadata-with-system-prompt`, or `:metadata-only`

## Installation

Add `agento11y-cl` to your ASDF system definition:

```lisp
:depends-on (:agento11y-cl ...)
```

## Quick start

```lisp
(defvar *client*
  (agento11y-cl:make-client
   (agento11y-cl:make-config
    :generation-endpoint "https://{your-agento11y-host}/api/v1/generations:export"
    :generation-enabled t
    :traces-endpoint "https://{your-otel-host}/v1/traces"
    :traces-enabled t
    :auth-mode :basic
    :auth-password "glc_..."
    :tenant-id "12345"
    :content-capture-mode :full
    :service-name "my-app")))

(agento11y-cl:client-start *client*)
```

### Record a generation

```lisp
(agento11y-cl:with-generation (rec *client*
                           :model-provider "anthropic"
                           :model-name "claude-sonnet-4-20250514"
                           :conversation-id "conv-123")
  ;; Call your LLM here, then record the result:
  (agento11y-cl:set-result rec
    :input-messages input-msgs
    :output-messages output-msgs
    :usage (agento11y-cl:make-token-usage :input 500 :output 200)
    :stop-reason "end_turn"))
```

Child tool executions and embeddings within the body are automatically parented to the generation's trace:

```lisp
(agento11y-cl:with-generation (gen *client* :model-provider "openai" :model-name "gpt-4")
  ;; ... LLM call ...
  (agento11y-cl:with-tool-execution (tool *client*
                                 :tool-name "web-search"
                                 :tool-call-id "tc_1")
    ;; ... execute tool ...
    (agento11y-cl:set-result tool :result "search results here")))
```

### Record an embedding

```lisp
(agento11y-cl:with-embedding (rec *client*
                          :model-provider "openai"
                          :model-name "text-embedding-3-small"
                          :dimensions 1536
                          :encoding-format "float")
  ;; ... call the embedding API ...
  (agento11y-cl:set-result rec
    :input-count 2
    :input-tokens 24
    :dimensions 1536
    :response-model "text-embedding-3-small"
    :input-texts '("first document" "second document")))
```

`:dimensions` and `:encoding-format` describe the request. `:response-model`
and the `set-result` `:dimensions` describe the response; a result dimension
count wins over the requested one on the span.

`:input-texts` reaches the span as `gen_ai.embeddings.input_texts` only when
`:embedding-capture-input` is enabled and the content capture mode is `:full`
or `:no-tool-content`. The SDK keeps the first `:embedding-max-input-items`
texts in order and cuts each one to `:embedding-max-text-length` characters.

### Message parts

A message carries a list of parts. There are five kinds:

| Constructor | Wire field | Notes |
|-------------|-----------|-------|
| `make-text-part` | `text` | Plain text |
| `make-thinking-part` | `thinking` | Reasoning text |
| `make-tool-call-part` | `tool_call` | `:id`, `:name`, `:input-json` |
| `make-tool-result-part` | `tool_result` | `:tool-call-id`, `:name`, `:content`, `:is-error` |
| `make-media-part` | `media` | Images and other non-text content |

`make-media-part` takes `:kind`, `:url`, `:mime-type`, `:name`, and `:provider-type`.
The four string fields default to `""`; `:provider-type` defaults to `nil`.

```lisp
(agento11y-cl:make-media-part :kind "image"
                          :url "https://example.com/chart.png"
                          :mime-type "image/png"
                          :name "chart.png"
                          :provider-type "image")
```

The URL can hold the bytes inline as a `data:` URI. `:provider-type` sets the
part's `metadata.provider_type`; when it is `nil` or empty the SDK omits the
`metadata` object. Under `:metadata-only` and `:metadata-with-system-prompt` the
SDK clears `url` and keeps `kind`, `mime_type`, and `name`.

### Normalize API responses

Convert raw LLM API hash-tables into SDK types:

```lisp
;; Anthropic/OpenAI message arrays -> CLOS message objects
(let* ((system (agento11y-cl:extract-system-prompt api-messages))
       (input  (agento11y-cl:normalize-input-messages api-messages))
       (output (agento11y-cl:build-output-message
                :text response-text
                :reasoning thinking-text
                :tool-calls tool-call-list)))
  (agento11y-cl:set-result rec
    :system-prompt system
    :input-messages input
    :output-messages (list output)))
```

Anthropic `image` blocks and OpenAI `image_url` blocks become media parts with
`kind` and `provider_type` set to `"image"`. An inline Anthropic source becomes a
`data:<mime>;base64,<data>` URL; a URL source passes through unchanged. A block
with neither a URL nor both a media type and inline data is dropped.

### Synchronous hook evaluation

`evaluate-hook` performs a synchronous `POST /api/v1/hooks:evaluate` against the hooks API before (preflight) or after (postflight) an upstream LLM call. The hook is the SDK's call; the rules it runs are the guards you configure in Grafana Cloud, which decide whether to allow, deny, or transform the input. The naming matches the Python, Go, and JavaScript SDKs.

Hooks are disabled by default. Nothing is sent until you pass `:hooks-config` with `:enabled t`:

```lisp
(defvar *client*
  (agento11y-cl:make-client
   (agento11y-cl:make-config
    :generation-endpoint "https://agento11y.example.com/api/v1/generations:export"
    ;; Optional: override the hooks API host root explicitly. When unset, the
    ;; hooks endpoint is derived from :generation-endpoint's host root.
    :api-endpoint "https://agento11y.example.com"
    :hooks-config (agento11y-cl:make-hooks-config :enabled t
                                              :phases '(:preflight)
                                              :timeout-sec 5.0
                                              :fail-open t)
    :auth-mode :basic
    :auth-password "glc_..."
    :tenant-id "12345")))

(handler-case
    (let* ((ctx (agento11y-cl:make-hook-context
                 :model-provider "anthropic"
                 :model-name "claude-sonnet-4-20250514"
                 :agent-name "router"
                 :tags '(("env" . "prod"))))
           (input (agento11y-cl:make-hook-input
                   :system-prompt system-prompt
                   :messages input-messages))
           (response (agento11y-cl:evaluate-hook *client*
                                             :phase :preflight
                                             :context ctx
                                             :input input)))
      ;; If the server returns transformed_input, substitute the rewritten
      ;; messages/tools/system-prompt into the upstream LLM call. The SDK
      ;; never mutates caller state -- the caller decides whether to use it.
      (let ((rewritten (agento11y-cl:response-transformed-input response)))
        (when rewritten
          (setf system-prompt (agento11y-cl:hook-input-system-prompt rewritten))
          (when (agento11y-cl:hook-input-messages rewritten)
            (setf input-messages (agento11y-cl:hook-input-messages rewritten)))))
      ;; ... call the upstream LLM ...
      )
  (agento11y-cl:agento11y-hook-denied-error (c)
    ;; Block the LLM call -- the server denied this request.
    (log-denied (agento11y-cl:agento11y-hook-denied-error-rule-id c)
                (agento11y-cl:agento11y-hook-denied-error-reason c)))
  (agento11y-cl:agento11y-hook-transport-error (c)
    ;; Only reachable when :fail-open nil -- otherwise transport errors
    ;; resolve to a synthetic allow response.
    (log-transport-failure c)))
```

Behaviour:

- **Allow** → returns a `hook-evaluate-response` with `(response-action r) = :allow`.
- **Deny** → signals `agento11y-hook-denied-error` with `rule-id`, `reason`, and per-rule `evaluations`.
- **Transport failure** → honours `(hooks-config-fail-open hooks)`: when `t` (default) returns a synthetic allow response so failed hook checks never block the LLM call; when `nil` signals `agento11y-hook-transport-error`. A fail-open allow is reported through `:log-fn` at `:warn` with component `"hooks"`, so an evaluator outage does not read as a clean allow.
- **transformed_input** → the response carries an optional `hook-input` accessible via `response-transformed-input`. Callers decide whether to substitute the rewritten `messages` / `tools` / `system_prompt` into the upstream call. The SDK does not mutate caller state.
- **Endpoint** → derived from `:api-endpoint` when set, otherwise from the host root of `:generation-endpoint`. Both `https://host` and `https://host/api/v1/...` forms are accepted; only the scheme + host are used. A schemeless or `grpc://` value contributes its host and resolves to `https://host`.
- **Timeout** → `(hooks-config-timeout-sec hooks)` (default `15.0`) is sent to the server via the `X-Agento11y-Hook-Timeout-Ms` header. The `:timeout-sec` keyword on `evaluate-hook` overrides it for a single call. Zero and negative values fall back to the default.
- **Response size** → bodies larger than 4 MiB are treated as transport failures.
- **Correlation** → `trace-id` and `span-id` on the context fall back to the ambient `*trace-context*`, so a hook called inside `with-generation` reports the same trace as the generation it guards. Set them (or `conversation-id`) explicitly to override.
- **Part encoding** → every message part carries its `kind`; the server dispatches on that field. Tool call arguments and tool result payloads go out as embedded JSON so rules can match on them, while a tool definition's `input_schema_json` stays base64, which is what the server's protobuf bytes field expects.

### Shutdown

```lisp
(agento11y-cl:client-shutdown *client*)
```

## Configuration

| Option | Default | Description |
|--------|---------|-------------|
| `:generation-endpoint` | `nil` | Full URL for generation export |
| `:generation-enabled` | `nil` | Enable generation recording |
| `:eval-endpoint` | `nil` | Base URL for experiment control-plane requests; falls back to the generation endpoint host |
| `:eval-path-prefix` | `"/api/v1"` | Path prefix for the experiment read routes. The `experiment-runs` write routes are absolute and ignore it |
| `:eval-auth-token` | `nil` | Bearer token for control-plane requests; replaces the generation auth headers when set. Score export always uses generation auth |
| `:scores-export-path` | `"/api/v1/scores:export"` | Score export route on the generation endpoint host |
| `:ingest-actor` | `"ingest:sdk/lisp"` | Value of the `X-Agento11y-Ingest-Actor` header. It is appended to both the eval auth and score-export auth headers, so it rides every eval request, reads included; `""` sends no header |
| `:experiment-url-template` | `nil` | Optional UI URL template with `{base}` and `{run_id}` placeholders |
| `:api-endpoint` | `nil` | Host root used to derive `/api/v1/...` URLs (currently the hooks endpoint). Falls back to the host of `:generation-endpoint` |
| `:hooks-config` | `nil` (hooks off) | Synchronous hook evaluation config. Opt in with `(make-hooks-config :enabled t :phases '(:preflight) :timeout-sec 15.0 :fail-open t)`; `make-hooks-config` itself defaults to `:enabled nil` |
| `:traces-endpoint` | `nil` | Full URL for OTLP trace export |
| `:traces-enabled` | `nil` | Enable trace/span export |
| `:metrics-endpoint` | `nil` | Full URL for OTLP metrics export (e.g. `https://{host}/v1/metrics`) |
| `:metrics-enabled` | `nil` | Enable built-in GenAI histogram metrics export |
| `:metrics-forward-auth` | `t` | Forward the same auth headers to the metrics endpoint |
| `:auth-mode` | `:none` | `:none`, `:basic`, `:bearer`, or `:tenant` |
| `:auth-user` | `nil` | Basic auth username (falls back to tenant-id) |
| `:auth-password` | `nil` | Auth password/token |
| `:tenant-id` | `nil` | Grafana Cloud tenant ID |
| `:extra-headers` | `nil` | Alist of extra HTTP headers merged with auth headers (user wins on case-insensitive collision) |
| `:content-capture-mode` | `:metadata-only` | `:full`, `:no-tool-content`, `:metadata-with-system-prompt`, or `:metadata-only` |
| `:embedding-capture-input` | `nil` | Put embedding input texts on the span as `gen_ai.embeddings.input_texts`. Also needs a content capture mode of `:full` or `:no-tool-content`; the other two modes suppress the texts. No environment variable sets this, matching the Go, Python, and JavaScript SDKs |
| `:embedding-max-input-items` | `20` | Number of input texts kept on the span. A zero or negative value falls back to 20 |
| `:embedding-max-text-length` | `1024` | Characters kept per input text. If the length is above 3, longer text keeps its first `length - 3` characters plus `...`. If the length is 3 or less, the text is cut with no suffix. A zero or negative value falls back to 1024 |
| `:batch-size` | `20` | Generations per export batch |
| `:flush-interval-sec` | `5` | Background flush interval |
| `:queue-max` | `500` | Generation queue capacity |
| `:trace-queue-max` | `nil` | Trace queue capacity (defaults to queue-max) |
| `:service-name` | `"unknown"` | Service name (OTLP resource attribute `service.name`) |
| `:service-version` | `nil` | Service version (OTLP resource attribute `service.version`) |
| `:agent-name` | `nil` | Agent name for `gen_ai.agent.name` span attribute and generation `agent_name` field; falls back to `service-name` |
| `:agent-version` | `nil` | Agent version for `gen_ai.agent.version` span attribute and generation `agent_version` field; falls back to `service-version` |
| `:user-id` | `nil` | User identifier (OTel `user.id` span attribute) |
| `:tags` | `nil` | Alist of tags applied to generation payloads |
| `:debug` | `nil` | Debug flag (also settable via `AGENTO11Y_DEBUG`) |
| `:log-fn` | `nil` | `(lambda (level component message) ...)` |
| `:metrics-fn` | `nil` | `(lambda (type recorder) ...)` |

### Environment variables

`make-client` automatically layers `AGENTO11Y_*` environment variables on top of
the supplied config: explicit caller config wins over env, env wins over
schema defaults. This matches the canonical agento11y SDKs schema (Go, Python, JS).

Every variable also has a legacy `SIGIL_<SUFFIX>` spelling. The `AGENTO11Y_`
name wins when both are set, and the choice is made before parsing, so a stale
legacy value cannot resurface when the preferred one fails validation. The two
spellings are never merged: the selected value is used whole, including for
`TAGS` and `HEADERS`.

| Variable | Config slot | Notes |
|----------|-------------|-------|
| `AGENTO11Y_ENDPOINT` | `:generation-endpoint` | Full URL; agento11y-cl is HTTP-only, no scheme auto-prepend |
| `AGENTO11Y_EVAL_ENDPOINT` | `:eval-endpoint` | Base URL for experiment control-plane requests |
| `AGENTO11Y_EVAL_PATH_PREFIX` | `:eval-path-prefix` | Defaults to `/api/v1` |
| `AGENTO11Y_EVAL_AUTH_TOKEN` | `:eval-auth-token` | Sent as `Bearer` on control-plane requests, replacing generation auth; not used for score export |
| `AGENTO11Y_INGEST_ACTOR` | `:ingest-actor` | Defaults to `ingest:sdk/lisp` |
| `AGENTO11Y_EXPERIMENT_URL_TEMPLATE` | `:experiment-url-template` | Supports `{base}` and `{run_id}` |
| `AGENTO11Y_HEADERS` | `:extra-headers` | `k=v,k2=v2`; merged into auth headers (user header wins on case-insensitive collision) |
| `AGENTO11Y_AUTH_MODE` | `:auth-mode` | `none` / `tenant` / `bearer` / `basic`; unknown values warn and are ignored |
| `AGENTO11Y_AUTH_TENANT_ID` | `:tenant-id` | |
| `AGENTO11Y_AUTH_TOKEN` | `:auth-password` | Used as bearer token or basic password |
| `AGENTO11Y_AGENT_NAME` | `:agent-name` | |
| `AGENTO11Y_AGENT_VERSION` | `:agent-version` | |
| `AGENTO11Y_USER_ID` | `:user-id` | |
| `AGENTO11Y_TAGS` | `:tags` | `k=v,k2=v2`; env is the base layer, caller-supplied tags win on key collision |
| `AGENTO11Y_CONTENT_CAPTURE_MODE` | `:content-capture-mode` | Accepts `full` / `no_tool_content` / `metadata_only`; unknown values warn and are ignored. `:metadata-with-system-prompt` is a code-only extension. An unsupported caller keyword warns and falls back to `:metadata-only` |
| `AGENTO11Y_DEBUG` | `:debug` | `1` / `true` / `yes` / `on` → t, otherwise nil |
| `AGENTO11Y_ENABLE_EXPERIMENTAL_FEATURES` | `:experimental-features` | Same truthy values as `AGENTO11Y_DEBUG` |

`AGENTO11Y_PROTOCOL` is not supported (agento11y-cl is HTTP-only); a warning is
logged when set to anything other than `http`/`https`. `AGENTO11Y_INSECURE` is a
no-op because TLS is controlled by the URL scheme.

> **Caveat: env can override caller defaults for a few slots.** The resolver
> cannot distinguish "caller passed the schema default" from "caller never set
> the slot". Practically this affects two security-sensitive options:
>
> - `:auth-mode :none` — `AGENTO11Y_AUTH_MODE` will replace it. A caller asking
>   for "no auth" can have credentials added by env.
> - `:content-capture-mode :metadata-only` — `AGENTO11Y_CONTENT_CAPTURE_MODE`
>   will replace it. A caller relying on `:metadata-only` to keep
>   prompt/response text out of telemetry can be silently downgraded to `full`
>   by the environment.
>
> If a deployment relies on these defaults for privacy or auth posture,
> either set the matching variable to the desired value or unset it
> before constructing the client. `make-client` accepts `:env-fn` for callers
> that want to ignore the host environment entirely (e.g. tests).
>
> `:eval-path-prefix` and `:ingest-actor` are not affected: their slots hold
> NIL until a caller sets them, so an explicit value equal to the default
> still wins over env.

### Service vs agent identity

`:service-name` / `:service-version` populate the OTLP resource attributes
`service.name` and `service.version` (i.e. who runs the SDK). `:agent-name` /
`:agent-version` populate the `gen_ai.agent.name` / `gen_ai.agent.version`
span attributes and the generation payload's `agent_name` / `agent_version`
fields (i.e. which logical agent produced the call). When a recorder is
created without explicit agent fields, the SDK falls back to config-level
agent fields and finally to the service fields, so applications that only set
`:service-name` keep their previous behaviour.

### Content capture modes

| Field | `:full` | `:no-tool-content` | `:metadata-with-system-prompt` | `:metadata-only` |
|-------|---------|--------------------|--------------------------------|------------------|
| Message text | full | full | empty string | empty string |
| Thinking text | full | full | empty string, part kept | empty string, part kept |
| Tool call input, tool result content | full | full | empty string | empty string |
| Media URL | full | full | empty string (kind, MIME type, name kept) | empty string (kind, MIME type, name kept) |
| System prompt | full | full | full | omitted |
| Conversation title | full | full | omitted | omitted |
| Tool `description`, `input_schema_json` | full | full | empty string (`name`, `type`, `deferred` kept) | empty string (`name`, `type`, `deferred` kept) |
| Rating comment | sent | sent | omitted | omitted |
| `call_error`, workflow `error`, span status | full | full | error category | error category |
| Tool span `gen_ai.tool.description` | full | full | omitted | omitted |
| Tool span args/results | full | `<redacted>` | `<redacted>` | `<redacted>` |
| Embedding input texts | when `:embedding-capture-input` | when `:embedding-capture-input` | omitted | omitted |

Message and part structure survives every mode: a redacted part is exported with
empty content rather than dropped, so part counts and roles stay comparable
across modes.

`:no-tool-content` matches the Go SDK's `ContentCaptureModeNoToolContent` semantics: keep generation content for evaluation, but redact tool execution span attributes (where untrusted tool I/O accumulates).

When a mode withholds error text, the SDK exports the classified error category
(`rate_limit`, `auth_error`, `server_error`, `timeout`, `client_error`, or
`sdk_error`) instead of the provider's message, so consumers keep the
classification. Error text follows the content gate on every span type,
including the tool execution span: `:no-tool-content` drops tool arguments and
results but keeps the error message.

`submit-conversation-rating` logs a warning when the capture mode drops the
comment. The POST still succeeds, and the default mode is `:metadata-only`, so a
caller who never set a mode would otherwise see the feedback text disappear
without a signal.

Every exported generation carries the tag
`agento11y.sdk.content_capture_mode`, holding `full`, `no_tool_content`, or
`metadata_only`. The server reads it to tell a stripped generation from a
full one: it collapses stripped generations in conversation transcripts, skips
them as per-generation judge variables, and warns on test-case promotion. The
SDK sets the tag last, so a caller tag using the same key cannot override it.
`:metadata-with-system-prompt` reports `metadata_only` because it strips all
message content and `metadata_only` is the value the backend acts on.

A `:content-capture-mode` outside the four supported keywords is treated as
`:metadata-only`. `resolve-config-from-env` also logs a warning naming the
rejected value, and serialization redacts independently of that warning, so a
config built directly with `make-config` still fails closed.

### Metrics

With `:metrics-enabled t` and a `:metrics-endpoint` set, the SDK aggregates four
GenAI histograms in memory and periodically POSTs them as an OTLP
`resourceMetrics` payload to the metrics endpoint, using cumulative aggregation
temporality (suitable for Mimir/Prometheus ingestion). Export cadence follows
`:flush-interval-sec`; a final export runs on shutdown. Metrics are disabled by
default and independent of the user `:metrics-fn` callback.

| Metric | Unit | Buckets |
|--------|------|---------|
| `gen_ai.client.operation.duration` | `s` | duration |
| `gen_ai.client.token.usage` | `token` | token |
| `gen_ai.client.time_to_first_token` | `s` | duration |
| `gen_ai.client.tool_calls_per_operation` | `count` | `[0,1,2,4,8,16,32,64]` |

Duration and token bucket boundaries match the current OTel GenAI semantic
conventions. `gen_ai.client.time_to_first_token` and
`gen_ai.client.tool_calls_per_operation` are SDK-custom metric names, not part
of the OTel spec; they preserve parity with the reference SDK's wire output. The
spec's standardized TTFT equivalent is
`gen_ai.client.operation.time_to_first_chunk` (Development stability). The
tool-call buckets are a deliberate divergence from the reference (which relies on
OTel's default `[0..10000]` set, wrongly scaled for tool counts).

Since Common Lisp has no OpenTelemetry SDK to delegate to, the aggregation and
OTLP serialization are hand-rolled here; the reference SDK gets these for free
from the OTel `Meter`.

## Experiments

An offline evaluation publishes an **experiment run**, one **trial** per test
case attempt, and **scores** attached to those trials.

Writes and reads use two different route families:

| | Route |
|---|---|
| Create or claim a run | `POST /api/v1/experiment-runs:upsert` |
| Finalize a run | `POST /api/v1/experiment-runs/{id}:finalize` |
| Open a trial | `POST /api/v1/experiment-runs/{id}/trials` |
| Close a trial | `PATCH /api/v1/experiment-runs/{id}/trials/{trial_id}` |
| Publish scores | `POST /api/v1/scores:export` |
| Read a run, its report, its scores | `GET /api/v1/eval/experiments/{id}[/report\|/scores]` |

Use `with-experiment` to open a run. Generations recorded inside the dynamic
extent are tagged with `experiment.run_id`, include `experiment_run_id`
metadata, and are captured so scores can attach to them.

```lisp
(agento11y-cl:with-experiment (run client :run-id "exp-prompt-a" :name "prompt A")
  (agento11y-cl:with-trial (trial run "case-1")
    (let ((rec (agento11y-cl:start-generation client
                 :model-provider "anthropic"
                 :model-name "claude-sonnet"
                 :input-messages input)))
      ;; Call the model, then record the result.
      (agento11y-cl:set-result rec :output-messages output :usage usage)
      (agento11y-cl:recorder-end rec)
      (agento11y-cl:trial-bind-generation trial (agento11y-cl:gen-rec-generation-id rec)))

    (agento11y-cl:trial-add-scores
     trial
     (list (agento11y-cl:make-score :evaluator-id "verifier"
                                :evaluator-version "1"
                                :score-key "final"
                                :value 0.9
                                :passed t)))))
```

`with-experiment` closes every open trial, then finalizes the run `completed`
on normal exit and `failed` on an error or a non-local exit such as an
interrupt. `"succeeded"` is accepted as an alias for `"completed"`; any other
status signals `agento11y-validation-error` before a request goes out.

The upsert route claims an existing run idempotently, so a rerun with the same
`:run-id` continues it. A `409 Conflict` means the run is in a state the
backend will not reclaim; the default `:on-conflict :reopen` proceeds anyway,
and `:on-conflict :error` re-signals.

### Trials

A trial is one attempt at one test case. Its id is deterministic —
`(stable-id "trial" experiment-id test-case-id attempt)` — so a rerun addresses
the same trial instead of creating a duplicate. Opening the same
`(test-case-id, attempt)` pair twice on one run signals `agento11y-validation-error`
before any request; bump `:attempt` for a retry.

```lisp
(agento11y-cl:with-trial (trial run "case-1" :attempt 2)
  ...)
```

Scores anchor to the trial, so a generation is optional. Bind one with
`trial-bind-generation` when there is a model call worth linking, and bind the
conversation with `trial-bind-conversation`; both are local setters that issue
no request.

A trial closes `completed` unless its body raised, in which case it closes
`failed` carrying the error text. The pass/fail verdict is not the trial
status — it belongs in the final score's `:passed`, which is what drives the
report's pass rate.

### Upload modes

`:upload` controls when scores reach the server:

| Mode | `experiment-run-add-scores` (no trial) | On normal exit |
|------|-----------------------------|----------------|
| `:continuous` (default) | exports immediately, returns accepted count | finalized `completed` |
| `:bulk` | buffers, returns buffered count | buffered scores published, then finalized `completed` |
| `:manual` | buffers, returns buffered count | left open; call `experiment-run-publish` then `experiment-run-finalize` yourself |

Scores added to a trial are always held on it and flushed when the trial
closes, ahead of the closing `PATCH`. In `:continuous` mode that flush exports
them; in `:bulk` and `:manual` it moves them to the run buffer.

`experiment-run-buffered-score-count` reports what is waiting;
`experiment-run-publish` exports the buffer and returns the newly accepted
count. Score ids are deterministic (`stable-id`, SHA-1, identical to the
Go/Python SDKs): the first score for a `(score-key, evaluator-id)` pair on a
trial is `(stable-id "score" experiment-id trial-id score-key evaluator-id)`,
and a repeat appends an occurrence counter, so rescoring produces a distinct
durable id and re-publishing after a partial failure dedupes server-side.

If a trial cannot be closed, the run still attempts the remaining closes, then
finalizes `failed` and omits `score_count` — a partial run has no trustworthy
local count.

### Built-in evaluators

An evaluator grades one output in this process and returns an
`evaluation-result`. `trial-record-evaluation` turns that result into a score on
a trial.

The LLM judge calls no provider itself: you pass an `:invoke` closure that takes
the rendered prompt and returns the reply text, plus an optional `token-usage`
as a second value. The dependency list stays as it is, and any client adapts in
a few lines.

```lisp
(let ((judge (agento11y-cl:make-llm-judge
              :evaluator-id "helpfulness"
              :model-name "claude-sonnet-4-5"
              :model-provider "anthropic"
              :invoke (lambda (prompt)
                        (values (my-llm-call prompt) (my-usage))))))
  (agento11y-cl:with-trial (trial run "case-1")
    (agento11y-cl:trial-bind-conversation trial conversation-id)
    (agento11y-cl:trial-record-evaluation
     trial
     (agento11y-cl:evaluate-output judge (list :input question
                                           :output answer
                                           :expected reference)))))
```

The default prompt asks for `{"score": <0..1>, "passed": <boolean>,
"explanation": "<brief reason>"}`. The parser finds complete top-level JSON
objects inside surrounding prose, takes the last one carrying a numeric `score`,
clamps it to `[0,1]`, and derives `passed` from the threshold (0.5 by default)
unless the reply sets `passed` or `pass`. `explanation` falls back to `reason`.
Pass `:parser` to replace the whole step.

`make-regex-judge` is the deterministic counterpart. Its value is the boolean.

```lisp
(agento11y-cl:make-regex-judge :evaluator-id "no-secrets"
                           :pattern "sk-[A-Za-z0-9]+"
                           :negate t)
```

`:full-match` requires the leftmost match to span the whole output, which is
what `cl-ppcre:scan` reports and what Go does. Python's `re.fullmatch` backtracks
inside the pattern to find a match that spans the whole string, so it accepts
patterns this leftmost-match check rejects.

`trial-record-evaluation` records the judge's own generation first, under ids
derived from `(stable-id "grader" run-id trial-id score-key evaluator-id)`, and
exports it before building the score. A rescore appends the same occurrence
counter the score id uses, so the second grading writes its own grader
generation instead of overwriting the first. That generation is deliberately not
registered as one the experiment run produced, so it never lands in the graded
score's generation attribution. Pass `:publish-grader nil` to skip it. Go seeds
its grader ids from the minted score id instead, so the two SDKs do not produce
identical grader ids for the same score.

### Cloud trial evaluation (experimental)

`trial-evaluate` grades a trial's bound conversation with an evaluator stored in
your tenant, instead of a score computed locally.

```lisp
(agento11y-cl:trial-bind-conversation trial conversation-id)
(agento11y-cl:trial-evaluate trial "my-stored-evaluator" :timeout-sec 120)
```

It persists the conversation binding, flushes pending generations so the
evaluator can read what it is asked to grade, queues the evaluation, then polls
until the status is `success` or `failed`. The poll interval starts at 0.5s and
doubles up to 5s. Worker failure signals `agento11y-trial-evaluation-failed-error`
and an exceeded deadline signals `agento11y-trial-evaluation-timeout-error`; both
carry the evaluation id, and the evaluation keeps running server-side either
way, so triggering the same combination again returns the same row.

Queuing an evaluation makes the owning run omit `score_count` when it finalizes:
the evaluator writes a score this process never counted, and asserting the local
total against it answers `409 score-count-mismatch`. A trial opened outside a
run cannot mark anything, so that caller must pass `:score-count nil` to
`experiment-run-finalize` themselves.

`trigger-trial-evaluation` and `get-trial-evaluation` are the transport calls
underneath, for a caller who wants to queue an evaluation without blocking.

All three are gated: without the experimental flag they signal
`agento11y-experimental-disabled-error` and send no request. Set it on the config
directly or through the environment:

```lisp
(agento11y-cl:make-config :experimental-features t ...)
```

### Trial artifacts

`trial-artifact` attaches a file, text, or raw bytes to a trial. Supply exactly
one of `:content`, `:text`, or `:path`.

```lisp
(agento11y-cl:trial-artifact trial :name "transcript" :text conversation-log)
(agento11y-cl:trial-artifact trial :name "screenshot" :path "/tmp/shot.png")
```

`:kind` is inferred from the MIME type when unset, and the MIME type is inferred
from the file extension for `:path`. The content posts as the raw request body;
`name`, `kind`, and `mime` ride the query string.

> **This SDK does not redact artifacts.** Go and Python strip secrets from
> text-like artifacts by default. agento11y-cl has no secret sanitizer yet, so
> content uploads exactly as supplied. Code ported from Python that relied on
> the default redaction has to strip secrets itself.

### Local suites

`test-suite` and `test-case` describe an evaluation locally. There is no YAML
loader and no stored-suite control plane; build suites in Lisp from plists,
alists, or parsed JSON.

```lisp
(let ((suite (agento11y-cl:make-test-suite
              :suite-id "suite-1" :version "v2"
              :cases (list (agento11y-cl:make-test-case :id "case-1"
                                                    :input "2+2?"
                                                    :expected "4"
                                                    :tags '("math"))))))
  (agento11y-cl:with-experiment (run client :run-id "exp-1" :name "suite run"
                                 :suite suite)
    (agento11y-cl:with-trial (trial run "case-1")
      ...)))
```

The run payload carries the suite's `suite_id` and `suite_version`, and each
trial stores a snapshot of its case. Snapshot `input` and `expected` are always
JSON objects: a scalar is wrapped as `{"value": ...}`, a mapping (a hash table,
an alist, or a plist) is kept as is.

Evaluator provenance travels as the `:evaluator-id` and `:evaluator-version`
strings on `make-score`. There is no evaluator value type, because no route
accepts one.

### Dataset runner

`run-experiment` loops a dataset through a target function and scorers under
one run, creating one trial per item:

```lisp
(agento11y-cl:run-experiment
 client
 (list (agento11y-cl:make-dataset-item :id "it1" :input "2+2?")
       (agento11y-cl:make-dataset-item :id "it2" :input "3+3?"))
 ;; target: run the agent for one item; generations recorded here are
 ;; captured and tagged automatically
 (lambda (item run)
   (declare (ignore run))
   (my-agent (agento11y-cl:jget item "input"))
   nil)                                  ; or (make-target-result ...)
 ;; scorers: grade one item, return a list of score outputs
 (list (lambda (item result)
         (declare (ignore item result))
         (list (agento11y-cl:make-score :evaluator-id "judge"
                                    :evaluator-version "1"
                                    :score-key "quality"
                                    :value 1.0))))
 :run-id "exp-prompt-a" :name "prompt A")
```

Each item also gets a stable per-item conversation id
(`(stable-id "conv" run-id item-id)`), so generations and scores link up in the
UI and reruns are idempotent. Item ids must be distinct: two items with the
same id would mint the same trial id. The return value is a plist with
`:run-id`, `:accepted-scores`, `:url`, and `:report`.

### Datasets from collections

`dataset-from-collection` turns a collection of saved conversations into
dataset items by fetching each conversation and recovering its initial user
prompt. The underlying read calls (`list-collection-members`,
`get-conversation`, `initial-user-prompt`) are also exported:

```lisp
(let ((items (agento11y-cl:dataset-from-collection client "col-123")))
  (agento11y-cl:run-experiment client items target scorers
                           :run-id "exp-col" :name "collection eval"
                           :collection-id "col-123"))
```

The upsert route rejects a `collection_id` field, so `:collection-id` is
carried as a `collectionId:` tag and a metadata key instead.

### Threads

`agento11y-cl:*experiment-run*` and `agento11y-cl:*trace-context*` are thread-confined
dynamic bindings. A thread you spawn starts at their global value `NIL`, so a
generation recorded there is neither tagged with the run nor tracked for score
attribution, and its span starts a new trace instead of joining the current
one. Carry both across with `telemetry-context-thunk`:

```lisp
(agento11y-cl:with-experiment (run client :run-id "exp-1" :name "threaded")
  (agento11y-cl:with-generation (parent client :mode :sync
                                    :model-provider "openai" :model-name "gpt-4")
    (let ((worker (bt2:make-thread
                   (agento11y-cl:telemetry-context-thunk
                    (lambda ()
                      (let ((rec (agento11y-cl:start-generation
                                  client :mode :sync
                                  :model-provider "openai" :model-name "gpt-4")))
                        (agento11y-cl:recorder-end rec)
                        (agento11y-cl:gen-rec-generation-id rec)))))))
      (bt2:join-thread worker))))
```

`telemetry-context-thunk` captures on the thread that calls it, which is the
point: a capture written inside the spawned lambda runs after the child already
started at `NIL` and carries nothing. For a caller that keeps its own list of
specials to rebind, `capture-telemetry-context` returns the snapshot and
`with-telemetry-context` rebinds it:

```lisp
(let ((context (agento11y-cl:capture-telemetry-context)))   ; on the parent
  (bt2:make-thread
   (lambda ()
     (agento11y-cl:with-telemetry-context (context)         ; on the child
       ...))))
```

Capture is held per run, not per trial, so trials on one run must stay
sequential. Opening a trial clears the run's captured generation ids; if
another trial is still open at that moment its scores can attribute to the
wrong generations, and the SDK logs a warning naming both trials. Spawning
threads inside one trial is fine, which is what these helpers are for.

A captured context outlives the `with-experiment` scope it was taken in, so
join your threads before that scope exits. A generation recorded after the run
finalized is still tracked, but the SDK warns: the score count and the trial
statuses already reported were written without it.

### Errors

| Condition | Raised when |
|---|---|
| `agento11y-validation-error` | HTTP 400/422, or a local check such as an unknown finalize status or a duplicate trial attempt |
| `agento11y-not-found-error` | HTTP 404 |
| `agento11y-conflict-error` | HTTP 409; `agento11y-conflict-error-kind` classifies it |
| `agento11y-actor-mismatch-error` | HTTP 401 naming actor ownership; not retried |
| `agento11y-export-error` | any other non-2xx, rejected scores, or an evaluation response the SDK cannot act on |
| `agento11y-experimental-disabled-error` | an experimental call with the gate off; no request is sent |
| `agento11y-trial-evaluation-failed-error` | a cloud evaluation reported `failed`; carries the evaluation id and the worker's detail |
| `agento11y-trial-evaluation-timeout-error` | a cloud evaluation did not finish in time; carries the evaluation id |

`agento11y-conflict-error-kind` returns `:score-count-mismatch`,
`:running-trials`, `:pending-evaluations`, `:terminal`, `:immutable-field`,
`:open-draft`, or `:unknown`. `conflict-recoverable-p` says whether the caller
can fix it and retry. `classify-conflict` exposes the classifier directly.

`agento11y-trial-evaluation-error-id` reads the evaluation id off either evaluation
condition; `agento11y-trial-evaluation-error-detail` reads the worker's message off
the failure.

### Configuration

Experiment calls are synchronous. Configure them with `:eval-endpoint`,
`:eval-path-prefix`, `:eval-auth-token`, `:scores-export-path`,
`:ingest-actor`, and `:experiment-url-template`, or the matching
environment variables (`AGENTO11Y_EVAL_ENDPOINT`, `AGENTO11Y_EVAL_PATH_PREFIX`,
`AGENTO11Y_EVAL_AUTH_TOKEN`, `AGENTO11Y_INGEST_ACTOR`,
`AGENTO11Y_EXPERIMENT_URL_TEMPLATE`).

If `:eval-endpoint` is unset, the SDK derives the base URL from
`:generation-endpoint`. `:eval-auth-token` sends a separate `Bearer` token on
control-plane requests — useful when generation export uses tenant or basic
auth but the plugin API needs a service-account token. Score export is
intentionally independent of the `AGENTO11Y_EVAL_*` settings: scores are a tenant
ingest write that goes to the generation endpoint host with generation auth,
same as the reference SDKs.

Every eval request carries an ingest-actor header identifying the writer,
`X-Agento11y-Ingest-Actor: ingest:sdk/lisp` by default. It is appended to both
the eval auth and the score-export auth headers, so the reads carry it too.
Override the value with `:ingest-actor`; set it to `""` to send no header. A backend
that answers `401` naming actor ownership means another actor claimed the run.

## Running tests

```
make test
```

## License

Apache-2.0
