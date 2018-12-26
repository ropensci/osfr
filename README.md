# osfr2

[![Build Status](https://travis-ci.org/aaronwolen/osfr.svg?branch=master)](https://travis-ci.org/aaronwolen/osfr) [![Coverage status](https://codecov.io/gh/aaronwolen/osfr/branch/master/graph/badge.svg)](https://codecov.io/github/aaronwolen/osfr?branch=master)

## Overview

osfr provides a suite of functions for interacting with [OSF][osf] that is primarily focused on project management workflows. 

OSF (Open Science Framework, <https://osf.io>) is a free and open source project management repository designed to support researchers of all technical backgrounds. The service includes unlimited cloud storage and file version history, providing a centralized location for your research materials that can be kept private, shared with select collaborators, or made publicly available with citable DOIs. 

## Installation

This package is currently under heavy development and is apt to undergo significant changes until a stable version has been submitted to CRAN. You've been warned.

Also note this version of osfr has been temporarily renamed to *osfr2*.

``` r
# install.packages("devtools")
devtools::install_github("aaronwolen/osfr")
```

## Function names

All osfr function names start with the prefix `osf_` to avoid namespace clashes for common operations and improve discoverability when using IDEs with autocomplete functionality. Functions that perform common filesystem operations are named for their Unix counterparts. For example, `osf_mkdir()` is used to create new directories.

`osf_*()` functions return one of the 3 `osf_tbl` subclasses based on the 3 primary types of OSF entities: nodes, users, and files. For operations where more than 1 entity type could be returned, type-specific functions are provided. and named by adding a suffix based on the OSF entity type. For example when provided an `osf_tbl_node` representing an OSF project:

* `osf_ls_files()` lists the files/folders
* `osf_ls_nodes()` lists the project's sub-components

<!-- links -->
[osf]: https://osf.io "Open Science Framework"
[cos]: https://cos.io "Center for Open Science"
