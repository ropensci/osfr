# Tests

## Overview

Tests use [testthat](https://testthat.r-lib.org/) with [vcr](https://docs.ropensci.org/vcr/) for HTTP mocking. Recorded API responses (cassettes) are stored in `tests/cassettes/` as YAML files.

## Testing Modes

### Offline (default)

With cassettes present, most tests run without any API access or credentials. Tests that require a PAT are automatically skipped.

```sh
Rscript --no-save --no-restore -e 'testthat::test_local()'
```

### Live test server

Set `OSF_SERVER=test` and provide a PAT from [test.osf.io](https://test.osf.io) to run the full test suite, including tests that create or modify data. The downloading tests depend on pre-existing assets (projects, files) created by `data-raw/create-test-project.R`.

```sh
export OSF_PAT=<personal access token from test.osf.io>
export OSF_SERVER=test
Rscript --no-save --no-restore -e 'testthat::test_local()'
```

### Live production server

Set `OSF_PAT` to a production token without setting `OSF_SERVER`. Cassette-backed tests replay normally. Tests requiring authentication run against production. Tests that depend on pre-existing assets specific to test.osf.io are skipped.

```sh
export OSF_PAT=<personal access token from osf.io>
Rscript --no-save --no-restore -e 'testthat::test_local()'
```

## Skip Helpers

Skip helpers are defined in `helpers.R` and used throughout the test suite to manage which tests run in each mode.

- `skip_if_no_pat()` -- skips tests that require authenticated API access
- `skip_if_not_test_server()` -- skips tests that rely on pre-existing assets only available on test.osf.io (currently only `test-downloading.R`)
