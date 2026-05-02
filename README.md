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
| `:content-capture-mode` | `:metadata-only` | `:full`, `:no-tool-content`, `:metadata-with-system-prompt`, or `:metadata-only` |
| `:batch-size` | `20` | Generations per export batch |
| `:flush-interval-sec` | `5` | Background flush interval |
| `:queue-max` | `500` | Generation queue capacity |
| `:trace-queue-max` | `nil` | Trace queue capacity (defaults to queue-max) |
| `:service-name` | `"unknown"` | Service name in OTLP resource |
| `:log-fn` | `nil` | `(lambda (level component message) ...)` |
| `:metrics-fn` | `nil` | `(lambda (type recorder) ...)` |

### Content capture modes

| Mode | Generation messages | System prompt | call_error | Tool span args/results |
|------|---------------------|---------------|-----------|------------------------|
| `:full` | full | full | full | full |
| `:no-tool-content` | full | full | full | redacted |
| `:metadata-with-system-prompt` | full | full | redacted | redacted |
| `:metadata-only` | structure only, text empty | omitted | redacted | redacted |

`:no-tool-content` matches the Go SDK's `ContentCaptureModeNoToolContent` semantics: keep generation content for evaluation, but redact tool execution span attributes (where untrusted tool I/O accumulates).

## Running tests

```
make test
```

## License

Apache-2.0
