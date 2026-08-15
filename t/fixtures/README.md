# Vendored conformance fixtures

These files are copies of the cross-SDK contract held in
[grafana/agento11y](https://github.com/grafana/agento11y). They are the same
files the Go, Python, and JavaScript suites check themselves against, so a
divergence in this SDK shows up as a failing check instead of as wrong data on
a live backend.

Copied at agento11y commit `086b53f612e8d49a6f3525195897914427be6d78` (the last
commit that touched `conformance/`, `redaction/patterns.json`, or
`redaction/fixtures/`).

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

`conformance/pi-sessions/` is not vendored: it pins a pi plugin this repo does
not ship.

## Refreshing

```bash
make conformance-refresh                       # reads ~/projects/agento11y
make conformance-refresh AGENTO11Y_REPO=/path  # or another checkout
```

Update the commit above after a refresh.

## Do not edit a fixture to make a suite green

`conformance/hooks/README.md` and `conformance/experiments/README.md` upstream
both say why: the suites check themselves against the files rather than against
a running server, so an invented shape passes every SDK suite and fails only in
production. A shape change belongs upstream, run through the server's decoder
first.
