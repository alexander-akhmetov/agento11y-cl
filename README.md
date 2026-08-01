# sigil-cl

Common Lisp SDK for [Grafana Sigil](https://github.com/grafana/sigil-sdk) AI observability.

Sigil captures LLM generations, tool executions, and embeddings from your application and exports them as structured telemetry — generation payloads over HTTP and traces via OTLP.

## Features

- **Generation recording** — capture model calls with messages, token usage, tool definitions, and timing
- **Tool execution tracing** — record tool/function calls as child spans linked to parent generations
- **Embedding tracing** — track embedding API calls with input counts and token usage
- **Ad-hoc spans** — wrap arbitrary code blocks in OTel spans via `with-span`
- **Conversation ratings** — submit user feedback ratings to the Sigil API
- **Offline experiments** — create eval runs, record tagged generations, export scores, and loop a dataset through a target and scorers
- **Collection datasets** — read Sigil collections and conversations to build experiment datasets from saved conversations
- **Message normalization** — convert raw Anthropic and OpenAI API responses into SDK types
- **Background export** — batched, async HTTP export with exponential backoff retry
- **Content capture modes** — `:full`, `:no-tool-content`, `:metadata-with-system-prompt`, or `:metadata-only`

## Installation

Add `sigil-cl` to your ASDF system definition:

```lisp
:depends-on (:sigil-cl ...)
```

## Quick start

```lisp
(defvar *client*
  (sigil-cl:make-client
   (sigil-cl:make-config
    :generation-endpoint "https://{your-sigil-host}/api/v1/generations:export"
    :generation-enabled t
    :traces-endpoint "https://{your-otel-host}/v1/traces"
    :traces-enabled t
    :auth-mode :basic
    :auth-password "glc_..."
    :tenant-id "12345"
    :content-capture-mode :full
    :service-name "my-app")))

(sigil-cl:client-start *client*)
```

### Record a generation

```lisp
(sigil-cl:with-generation (rec *client*
                           :model-provider "anthropic"
                           :model-name "claude-sonnet-4-20250514"
                           :conversation-id "conv-123")
  ;; Call your LLM here, then record the result:
  (sigil-cl:set-result rec
    :input-messages input-msgs
    :output-messages output-msgs
    :usage (sigil-cl:make-token-usage :input 500 :output 200)
    :stop-reason "end_turn"))
```

Child tool executions and embeddings within the body are automatically parented to the generation's trace:

```lisp
(sigil-cl:with-generation (gen *client* :model-provider "openai" :model-name "gpt-4")
  ;; ... LLM call ...
  (sigil-cl:with-tool-execution (tool *client*
                                 :tool-name "web-search"
                                 :tool-call-id "tc_1")
    ;; ... execute tool ...
    (sigil-cl:set-result tool :result "search results here")))
```

### Normalize API responses

Convert raw LLM API hash-tables into SDK types:

```lisp
;; Anthropic/OpenAI message arrays -> CLOS message objects
(let* ((system (sigil-cl:extract-system-prompt api-messages))
       (input  (sigil-cl:normalize-input-messages api-messages))
       (output (sigil-cl:build-output-message
                :text response-text
                :reasoning thinking-text
                :tool-calls tool-call-list)))
  (sigil-cl:set-result rec
    :system-prompt system
    :input-messages input
    :output-messages (list output)))
```

### Shutdown

```lisp
(sigil-cl:client-shutdown *client*)
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
| `:debug` | `nil` | Debug flag (also settable via `SIGIL_DEBUG`) |
| `:log-fn` | `nil` | `(lambda (level component message) ...)` |
| `:metrics-fn` | `nil` | `(lambda (type recorder) ...)` |

### Environment variables

`make-client` automatically layers `SIGIL_*` environment variables on top of
the supplied config: explicit caller config wins over env, env wins over
schema defaults. This matches the canonical sigil-sdk schema (Go, Python, JS).

| Variable | Config slot | Notes |
|----------|-------------|-------|
| `SIGIL_ENDPOINT` | `:generation-endpoint` | Full URL; sigil-cl is HTTP-only, no scheme auto-prepend |
| `SIGIL_EVAL_ENDPOINT` | `:eval-endpoint` | Base URL for experiment control-plane requests |
| `SIGIL_EVAL_PATH_PREFIX` | `:eval-path-prefix` | Defaults to `/api/v1` |
| `SIGIL_EVAL_AUTH_TOKEN` | `:eval-auth-token` | Sent as `Bearer` on control-plane requests, replacing generation auth; not used for score export |
| `SIGIL_INGEST_ACTOR` | `:ingest-actor` | Defaults to `ingest:sdk/lisp` |
| `SIGIL_EXPERIMENT_URL_TEMPLATE` | `:experiment-url-template` | Supports `{base}` and `{run_id}` |
| `SIGIL_HEADERS` | `:extra-headers` | `k=v,k2=v2`; merged into auth headers (user header wins on case-insensitive collision) |
| `SIGIL_AUTH_MODE` | `:auth-mode` | `none` / `tenant` / `bearer` / `basic`; unknown values warn and are ignored |
| `SIGIL_AUTH_TENANT_ID` | `:tenant-id` | |
| `SIGIL_AUTH_TOKEN` | `:auth-password` | Used as bearer token or basic password |
| `SIGIL_AGENT_NAME` | `:agent-name` | |
| `SIGIL_AGENT_VERSION` | `:agent-version` | |
| `SIGIL_USER_ID` | `:user-id` | |
| `SIGIL_TAGS` | `:tags` | `k=v,k2=v2`; env is the base layer, caller-supplied tags win on key collision |
| `SIGIL_CONTENT_CAPTURE_MODE` | `:content-capture-mode` | Accepts `full` / `no_tool_content` / `metadata_only`; unknown values warn and are ignored. `:metadata-with-system-prompt` is a code-only extension |
| `SIGIL_DEBUG` | `:debug` | `1` / `true` / `yes` / `on` → t, otherwise nil |

`SIGIL_PROTOCOL` is not supported (sigil-cl is HTTP-only); a warning is logged
when set to anything other than `http`/`https`. `SIGIL_INSECURE` is a no-op
because TLS is controlled by the URL scheme.

> **Caveat: env can override caller defaults for a few slots.** The resolver
> cannot distinguish "caller passed the schema default" from "caller never set
> the slot". Practically this affects two security-sensitive options:
>
> - `:auth-mode :none` — `SIGIL_AUTH_MODE` will replace it. A caller asking
>   for "no auth" can have credentials added by env.
> - `:content-capture-mode :metadata-only` — `SIGIL_CONTENT_CAPTURE_MODE` will
>   replace it. A caller relying on `:metadata-only` to keep prompt/response
>   text out of telemetry can be silently downgraded to `full` by the
>   environment.
>
> If a deployment relies on these defaults for privacy or auth posture,
> either set the matching `SIGIL_*` var to the desired value or unset it
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

| Mode | Generation messages | System prompt | call_error | Tool span args/results |
|------|---------------------|---------------|-----------|------------------------|
| `:full` | full | full | full | full |
| `:no-tool-content` | full | full | full | redacted |
| `:metadata-with-system-prompt` | structure only, text empty | full | redacted | redacted |
| `:metadata-only` | structure only, text empty | omitted | redacted | redacted |

`:no-tool-content` matches the Go SDK's `ContentCaptureModeNoToolContent` semantics: keep generation content for evaluation, but redact tool execution span attributes (where untrusted tool I/O accumulates).

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
`gen_ai.client.tool_calls_per_operation` are Sigil-custom metric names, not part
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
(sigil-cl:with-experiment (run client :run-id "exp-prompt-a" :name "prompt A")
  (sigil-cl:with-trial (trial run "case-1")
    (let ((rec (sigil-cl:start-generation client
                 :model-provider "anthropic"
                 :model-name "claude-sonnet"
                 :input-messages input)))
      ;; Call the model, then record the result.
      (sigil-cl:set-result rec :output-messages output :usage usage)
      (sigil-cl:recorder-end rec)
      (sigil-cl:trial-bind-generation trial (sigil-cl:gen-rec-generation-id rec)))

    (sigil-cl:trial-add-scores
     trial
     (list (sigil-cl:make-score :evaluator-id "verifier"
                                :evaluator-version "1"
                                :score-key "final"
                                :value 0.9
                                :passed t)))))
```

`with-experiment` closes every open trial, then finalizes the run `completed`
on normal exit and `failed` on an error or a non-local exit such as an
interrupt. `"succeeded"` is accepted as an alias for `"completed"`; any other
status signals `sigil-validation-error` before a request goes out.

The upsert route claims an existing run idempotently, so a rerun with the same
`:run-id` continues it. A `409 Conflict` means the run is in a state the
backend will not reclaim; the default `:on-conflict :reopen` proceeds anyway,
and `:on-conflict :error` re-signals.

### Trials

A trial is one attempt at one test case. Its id is deterministic —
`(stable-id "trial" experiment-id test-case-id attempt)` — so a rerun addresses
the same trial instead of creating a duplicate. Opening the same
`(test-case-id, attempt)` pair twice on one run signals `sigil-validation-error`
before any request; bump `:attempt` for a retry.

```lisp
(sigil-cl:with-trial (trial run "case-1" :attempt 2)
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

### Local suites

`test-suite` and `test-case` describe an evaluation locally. There is no YAML
loader and no stored-suite control plane; build suites in Lisp from plists,
alists, or parsed JSON.

```lisp
(let ((suite (sigil-cl:make-test-suite
              :suite-id "suite-1" :version "v2"
              :cases (list (sigil-cl:make-test-case :id "case-1"
                                                    :input "2+2?"
                                                    :expected "4"
                                                    :tags '("math"))))))
  (sigil-cl:with-experiment (run client :run-id "exp-1" :name "suite run"
                                 :suite suite)
    (sigil-cl:with-trial (trial run "case-1")
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
(sigil-cl:run-experiment
 client
 (list (sigil-cl:make-dataset-item :id "it1" :input "2+2?")
       (sigil-cl:make-dataset-item :id "it2" :input "3+3?"))
 ;; target: run the agent for one item; generations recorded here are
 ;; captured and tagged automatically
 (lambda (item run)
   (declare (ignore run))
   (my-agent (sigil-cl:jget item "input"))
   nil)                                  ; or (make-target-result ...)
 ;; scorers: grade one item, return a list of score outputs
 (list (lambda (item result)
         (declare (ignore item result))
         (list (sigil-cl:make-score :evaluator-id "judge"
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
(let ((items (sigil-cl:dataset-from-collection client "col-123")))
  (sigil-cl:run-experiment client items target scorers
                           :run-id "exp-col" :name "collection eval"
                           :collection-id "col-123"))
```

The upsert route rejects a `collection_id` field, so `:collection-id` is
carried as a `collectionId:` tag and a metadata key instead.

### Errors

| Condition | Raised when |
|---|---|
| `sigil-validation-error` | HTTP 400/422, or a local check such as an unknown finalize status or a duplicate trial attempt |
| `sigil-not-found-error` | HTTP 404 |
| `sigil-conflict-error` | HTTP 409; `sigil-conflict-error-kind` classifies it |
| `sigil-actor-mismatch-error` | HTTP 401 naming actor ownership; not retried |
| `sigil-export-error` | any other non-2xx, or rejected scores |

`sigil-conflict-error-kind` returns `:score-count-mismatch`,
`:running-trials`, `:pending-evaluations`, `:terminal`, `:immutable-field`,
`:open-draft`, or `:unknown`. `conflict-recoverable-p` says whether the caller
can fix it and retry. `classify-conflict` exposes the classifier directly.

### Configuration

Experiment calls are synchronous. Configure them with `:eval-endpoint`,
`:eval-path-prefix`, `:eval-auth-token`, `:scores-export-path`,
`:ingest-actor`, and `:experiment-url-template`, or the matching `SIGIL_*`
environment variables (`SIGIL_EVAL_ENDPOINT`, `SIGIL_EVAL_PATH_PREFIX`,
`SIGIL_EVAL_AUTH_TOKEN`, `SIGIL_INGEST_ACTOR`,
`SIGIL_EXPERIMENT_URL_TEMPLATE`).

If `:eval-endpoint` is unset, the SDK derives the base URL from
`:generation-endpoint`. `:eval-auth-token` sends a separate `Bearer` token on
control-plane requests — useful when generation export uses tenant or basic
auth but the plugin API needs a service-account token. Score export is
intentionally independent of the `SIGIL_EVAL_*` settings: scores are a tenant
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
