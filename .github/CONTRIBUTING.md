# Contributing to osfr

This outlines how to propose a change to osfr.

### Development environment

To get started with osfr development you'll need to generate a personal access token (PAT) on OSF's *testing* server. The following steps will get you setup:

1. Create an account on <https://test.osf.io/>.
2. Generate a PAT for the new account with all permission scopes enabled (this is necessary to run osfr's unit tests). (Under "Settings"->Personal access token)
3. Fork the osfr repository and clone a local copy.
4. Create a `.Renviron` file in the root of your project directory that defines the `OSF_PAT` and `OSF_SERVER` environment variables. You can easily create or edit an existing `.Renviron` file by running `usethis::edit_r_environ(scope = "project")`. The end result should look like this:

   ```
   OSF_PAT=<YOUR PAT GOES HERE>
   OSF_SERVER=test
   ```

5. Restart R to load your `.Renviron` file or use `readRneviron(".Renviron")`.  Then load your local copy of osfr with `devtools::load_all()` and verify that `osf_open(osf_retrieve_user("me"))` opens your user profile on the `test.osf.io` domain.

Once this is setup correctly, you should be able to run osfr's tests without error (`devtools::test()`).

You can also enable logging by defining `OSF_LOG` to point to a logfile. For example:

```
OSF_PAT=osfr.log
```

This will log all API requests to `osfr.log` for inspection.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).
*  Look at the Travis and AppVeyor build status before and after making changes.
The `README` should contain badges for any continuous integration services used
by the package.
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
You can use the [styler](https://CRAN.R-project.org/package=styler) package to
apply these styles, but please don't restyle code that has nothing to do with
your PR.
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made followed by your
GitHub username, and links to relevant issue(s)/PR(s).

### Using the included `Makefile`

Run `make docs` to:
* rebuild `README.md` from `README.Rmd`
* regenerate the precomputed vignette `getting_started.Rmd` from `getting_started.Rmd.orig`
* rebuild the vignettes
* rebuild package documentation

Run `make` to
* perform all of the documentation steps noted above
* build the package
* check the package as CRAN but without running tests (this is temporary until mock tests are implemented)

**Helpers:**

* `make clean` to remove build/check files
* `make test` to run unit tests
* `make check` to run `R CMD check` on the package
* `make revdep` to run reverse dependency checks
* `make tag` to tag the last git commit with the current version
