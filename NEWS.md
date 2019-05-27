# osfr 0.2.4.9000


## Multi-file transfers!

`osf_download()` and `osf_upload()` are now vectorized, making the process of adding files to or retriving files from OSF much more convenient. This functionality required significant refactoring and brings with it several notable breaking changes. 

## Breaking changes

* `osf_upload()`'s `name` argument has been removed, so it is no longer possible to upload a file *and* change it's OSF name.

* `osf_download()`'s `path` argument must point to the directory where downloaded files should be saved. 

* `osf_upload()`'s behavior has changed when `overwrite = FALSE` and a file with the same name already exists on OSF. Previously, an error was thrown and the operation would cease. Now a *warning* is thrown and an `osf_tbl_file` containing the current version of the OSF file is returned. This change was made to facilitate multifile uploading, particularly situations where only subset of files already exist on OSF and you don't want them overwritten when uploading the missing files. 

## Minor changes

* Devs can now enable logging API requests and responses by defining defining
`OSF_LOG` (see Contributing for more information)

* Better error message when user attempts to upload directly to a file 
(#102, @tiernanmartin)

* crul v0.7.4 is now the minimum required version

* The waterbutler client will now re-attempt failed requests 3 times

# osfr 0.2.4

## Minor fixes

* Listing files within a specified `path` would fail if sibling directories
shared a common substring in their names (#95)
* Setting `verbose=TRUE` now works properly for `osf_upload()`
* A startup message is printed when `OSF_SERVER` is defined
* Improved documentation for `n_max`, GUIDs and the mysterious `meta` column

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
