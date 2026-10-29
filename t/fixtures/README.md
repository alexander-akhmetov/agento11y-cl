# Vendored conformance fixtures

These files are copies of the cross-SDK contract held in
[grafana/agento11y](https://github.com/grafana/agento11y). They are the same
files the Go, Python, and JavaScript suites check themselves against, so a
divergence in this SDK shows up as a failing check instead of as wrong data on
a live backend.

The hooks, experiments, and redaction fixtures were copied at agento11y commit
`086b53f612e8d49a6f3525195897914427be6d78`. The other sources have separate
commit records below.

| Vendored path | Upstream path |
|---|---|
| `hooks/request-preflight.json` | `conformance/hooks/request-preflight.json` |
| `hooks/request-postflight-guard.json` | `conformance/hooks/request-postflight-guard.json` |
| `hooks/responses.json` | `conformance/hooks/responses.json` |
| `experiments/inputs.json` | `conformance/experiments/inputs.json` |
| `experiments/ids.json` | `conformance/experiments/ids.json` |
| `experiments/requests.json` | `conformance/experiments/requests.json` |
| `experiments/responses.json` | `conformance/experiments/responses.json` |
| `redaction/strings.json` | `redaction/fixtures/strings.json` |
| `redaction/generations.json` | `redaction/fixtures/generations.json` |
| `redaction/patterns.json` | `redaction/patterns.json` |
| `otlpwire/*.generation.json` | `go/agento11y/testdata/otlpwire/*.generation.json` |
| `otlpwire/*.span.json` | `go/agento11y/testdata/otlpwire/*.span.json` |

`otlpwire/` differs from the other rows. The rest come from the shared
`conformance/` and `redaction/` directories, which are a cross-SDK contract.
The six `otlpwire/` files come from Go-private `testdata/`: they can change
upstream without notice. Python also checks its adapter against these files. They were
copied at agento11y commit `0137e0ef04f857c0922405c7a262800914f0a094`, the last
commit that touched them. Proposing them for a shared `conformance/genai/`
upstream is the right follow-up.

## Tool execution kinds

`tool-execution-kinds/cases.json` comes from
`conformance/tool-execution-kinds/cases.json` at agento11y commit
`5ed7a0fa02ef69dd34e5af5a68fdf7198df4fada`. This copy has separate provenance;
the hooks, experiments, redaction, and OTLP wire fixtures were not refreshed.

`run-tool-execution-conformance-tests` consumes all 103 cases with both `:http`
and `:otel` generation protocols. It compares exact skill/process subsets as
OTLP typed values, including absent attributes. It also checks duplicate keys,
span names, one INTERNAL span, and no generation payload or `agento11y.skill.name`.

The Lisp adapter explicitly maps fixture fields and execution kinds to API
keywords. An unknown execution kind reaches the API as `:future` to test generic
fallback. Unrestricted invalid exit codes must be omitted. Cases restricted by
`dynamic_languages` supply no exit code unless they name `lisp`.
The JavaScript-only unsafe integer therefore tests an absent code in Lisp.

The fixture's `include_content` and `otel_capture` fields describe legacy APIs,
not Lisp start keywords. Lisp uses `capture_mode`, defaulting to
`:no-tool-content`. Both protocols keep the Lisp capture policy;
`:full-with-metadata-spans` does not widen to `:full` on the OTel path.

`conformance/pi-sessions/` is not vendored: it pins a pi plugin this repo does
not ship.

## Refreshing

```bash
make conformance-refresh                       # reads ~/projects/agento11y
make conformance-refresh AGENTO11Y_REPO=/path  # or another checkout
```

Update each source's commit record after a refresh. Do not replace the separate
records with one checkout commit.

## Do not edit a fixture to make a suite green

`conformance/hooks/README.md` and `conformance/experiments/README.md` upstream
both say why: the suites check themselves against the files rather than against
a running server, so an invented shape passes every SDK suite and fails only in
production. A shape change belongs upstream, run through the server's decoder
first.
