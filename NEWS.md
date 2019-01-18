# osfr 0.2.1.9000

## New functions

* `osf_mv()` to move files and directories to a new project, component, or
subdirectory

## New features

* `osf_rm()` can now delete files and directories

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
