# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
make test                 # Run all tests (SBCL, requires Quicklisp)
make load                 # Compile-check: load the system without running tests
make clean                # Remove .fasl caches
make conformance-refresh  # Re-copy t/fixtures/ from an agento11y checkout
```

Tests use a custom framework in `t/suite.lisp` (no external test library). Each suite is a `defun` (e.g., `run-recorder-tests`) called by `run-tests`. Add new test cases inside existing suites using `(check "label" condition)`. The `make test` output is filtered to show only pass/fail lines.

The SBCL invocation uses `--no-userinit` and loads deps via Quicklisp. Set `QUICKLISP_HOME` if your Quicklisp is not at `~/quicklisp/`.

### Conformance suites

`t/conformance.lisp` checks this SDK against the cross-SDK fixtures vendored in `t/fixtures/`, in three suites: hooks, experiments, and redaction. `make conformance-refresh` re-copies the fixtures from an `agento11y` checkout, and `t/fixtures/README.md` maps each file to its upstream path and commit. Do not edit a fixture to make a suite green: a shape change belongs upstream in `grafana/agento11y`, run through the server's decoder first.

The redaction engine and the content-capture rules are behind the fixtures. Every redaction check that fails for that reason is listed by id in `+cf-known-redaction-failures+`, so `make test` stays green and any other failure turns it red. A listed check that starts passing fails too, naming the id to drop from the list.

## Architecture

Common Lisp SDK that captures LLM telemetry and exports it via two paths:
- **Generations** -- structured JSON payloads POSTed to the generation API
- **Traces** -- OTel spans POSTed to an OTLP-compatible traces endpoint

### Data flow

`Recorder` -> serialize -> enqueue -> background flush loop -> HTTP POST with retry

1. Caller creates a recorder via `start-generation`, `start-tool-execution`, or `start-embedding` (or the `with-*` macros)
2. Caller sets results/errors on the recorder
3. `recorder-end` serializes the telemetry and pushes it onto bounded queues in the client
4. A background thread (`run-flush-loop`) drains queues in batches and exports via `post-with-retry`

### Key design decisions

- **Two independent queues**: `client-generation-queue` (generation payloads) and `client-trace-queue` (OTel spans). Both are bounded and drop-oldest on overflow.
- **`*trace-context*`** (dynamic variable): `with-generation` binds this with the generation's trace-id/span-id. Child `with-tool-execution` and `with-embedding` calls read it to set their `parentSpanId`, creating the span tree.
- **Content capture modes** (`:full`, `:no-tool-content`, `:full-with-metadata-spans`, `:metadata-only`): control which content-bearing fields are serialized. Two surfaces are gated separately, so a mode needs two predicates: `capture-keeps-payload-content-p` for the generation payload and `capture-keeps-span-content-p` for OTel spans (with `capture-keeps-tool-span-content-p` stricter still, for tool call arguments and results). `:full-with-metadata-spans` keeps the payload and strips the spans. In `:metadata-only`, message structure is preserved but text and thinking fields are empty strings, tool inputs/results and media URLs are cleared, and the system prompt, conversation title, tool descriptions, tool input schemas, and rating comments are withheld. A redacting mode also removes the metadata keys the SDK mirrors content into (`+content-metadata-keys+`) whoever wrote them; no other caller metadata or tag is touched. `config.lisp` owns the vocabulary, so any mode outside the four keywords redacts. Withheld error text is exported as the classified error category, not a placeholder. Every generation carries the `agento11y.sdk.content_capture_mode` tag so the backend can identify stripped content.
- **`recorder-end :around`**: shared lifecycle logic (idempotency guard, timestamp, wake worker, metrics callback) lives in the `:around` method on the base `recorder` class. Type-specific serialization is in the primary methods.
- **`http-fn` config slot**: tests inject a lambda to capture HTTP requests instead of hitting the network.

### Module responsibilities

| File | Role |
|------|------|
| `config.lisp` | `agento11y-config` class, all tunables, the content-capture-mode vocabulary and predicates |
| `client.lisp` | `agento11y-client`, background flush loop, lifecycle, recorder factory functions |
| `recorder.lisp` | Base `recorder` class, `generation-recorder`, `tool-execution-recorder`, `embedding-recorder`, serialization |
| `macros.lisp` | `with-generation`, `with-tool-execution`, `with-embedding`, `with-span`; the telemetry context capture/replay helpers |
| `exporter.lisp` | HTTP POST with exponential backoff retry |
| `normalize.lisp` | Convert raw Anthropic/OpenAI API hash-tables into SDK CLOS types |
| `otel.lisp` | OTel attribute helpers, span/payload builders |
| `queue.lisp` | Thread-safe bounded queue (drop-oldest overflow) |
| `auth.lisp` | Build auth headers from config (basic/bearer/tenant) |
| `types.lisp` | CLOS message/part/token-usage types and constructors |
| `env.lisp` | Resolve `AGENTO11Y_*` environment variables (with `SIGIL_*` legacy fallback) into a config |
| `eval.lisp` | Experiment wire protocol: run upsert and finalize, score export, the read routes, ingest actor, HTTP status classification |
| `suite.lisp` | Local `test-suite` and `test-case` values; trial snapshots |
| `trial.lisp` | `experiment-trial`, `create-trial`, `finalize-trial`, `with-trial`, bind helpers |
| `experiment.lisp` | `experiment-run` orchestration: upload modes, score building, trial lifecycle, `run-experiment` |
| `conversations.lisp` | Read collections and conversations; build datasets from them |
| `metrics.lisp` | OTLP histogram registry and export |
| `rating.lisp` | Submit conversation ratings |

## Conventions

- JSON is built with `jobj`/`jarr`/`jget`/`jget*` helpers wrapping jzon hash-tables, not raw `make-hash-table` calls
- Package nicknames: `jzon` for `com.inuoe.jzon`, `bt2` for `bordeaux-threads-2`, `alex` for `alexandria`
- Timestamps are ISO 8601 strings (`iso8601-now`) or nanosecond strings (`current-unix-nano`) -- never fixnums
- Tool call `input_json` is base64-encoded in serialized output
- ASDF system loads files serially (`:serial t`); file order in the `.asd` matters
- Experiment writes go to `/api/v1/experiment-runs`; only reads use `/api/v1/eval/experiments`. See the comment block at the top of `eval.lisp` for the full contract and where it came from
- `*trace-context*` and `*experiment-run*` are the thread-propagated specials: callers carry them onto spawned threads with `capture-telemetry-context` / `with-telemetry-context` / `telemetry-context-thunk` (`macros.lisp`). New dynamic state that `start-generation` reads alongside either one must be added to both `capture-telemetry-context` (which snapshots it) and `with-telemetry-context` (which rebinds it); adding it to one alone still drops it at the thread hop
