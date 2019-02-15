# osfr 0.2.3.9000

## Minor fixes

* Listing files within a specified `path` would fail if sibling directories
shared a common substring in their names (#95)

# osfr 0.2.3

## New features

* Failed OSF API requests are now re-attempted 3 times (requires crul v0.7.0)

## Minor fixes

* Fix incorrect column name in empty `osf_tbl`s (#88, @machow)
* No longer importing `modify_at()`
* Add rOpenSci badge (#89, @maelle)
* Don't build vignettes on travis

# osfr 0.2.2

## New features

* Added `osf_mv()` to move files and directories to a new project, component, or
subdirectory
* `osf_rm()` can now delete files and directories

## Minor improvements and fixes

* Restructured tests to better handle environments in which `OSF_PAT` and/or `OSF_SERVER` are not defined

# osfr 0.2.1

* Minor tweaks to the website
* `osf_retrieve_file()` will no longer retrieve files on 3rd-party storage
providers, since other osfr functions currently only support OSF storage

# osfr 0.2.0

**NOTE:** This version of osfr is a rewrite of the original codebase. It is
effectively an entirely different package and provides no backwards
compatibility with functions in versions < 0.2.0. The last version of the
previous package can be installed with the *remotes* package:

```r
remotes::install_github("centerforopenscience/osfr@v0.1.1")
```

See <https://centerforopenscience.github.io/osfr/> for details about the new
package.
