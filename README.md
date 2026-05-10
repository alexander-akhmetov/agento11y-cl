# sigil-cl

Common Lisp SDK for [Grafana Sigil](https://github.com/grafana/sigil-sdk) AI observability.

Sigil captures LLM generations, tool executions, and embeddings from your application and exports them as structured telemetry — generation payloads over HTTP and traces via OTLP.

## Features

- **Generation recording** — capture model calls with messages, token usage, tool definitions, and timing
- **Tool execution tracing** — record tool/function calls as child spans linked to parent generations
- **Embedding tracing** — track embedding API calls with input counts and token usage
- **Ad-hoc spans** — wrap arbitrary code blocks in OTel spans via `with-span`
- **Conversation ratings** — submit user feedback ratings to the Sigil API
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
| `:traces-endpoint` | `nil` | Full URL for OTLP trace export |
| `:traces-enabled` | `nil` | Enable trace/span export |
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

## Running tests

```
make test
```

## License

Apache-2.0
