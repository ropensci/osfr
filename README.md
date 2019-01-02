
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osfr2

[![Build
Status](https://travis-ci.org/aaronwolen/osfr.svg?branch=master)](https://travis-ci.org/aaronwolen/osfr)
[![Coverage
status](https://codecov.io/gh/aaronwolen/osfr/branch/master/graph/badge.svg)](https://codecov.io/github/aaronwolen/osfr?branch=master)

## Overview

osfr provides a suite of functions for interacting with
[OSF](https://osf.io "Open Science Framework") that are primarily
focused on project management workflows.

OSF (Open Science Framework, <https://osf.io>) is a free and [open
source](https://github.com/CenterForOpenScience/osf.io "OSF's GitHub Repository")
project management repository designed to support researchers of all
technical backgrounds. The service includes unlimited cloud storage and
file version history, providing a centralized location for your research
materials that can be kept private, shared with select collaborators, or
made publicly available with citable DOIs.

OSF is developed by the [Center for Open
Science](https://cos.io "Center for Open Science") in Charlottesville,
VA.

## Installation

This package is currently under heavy development and is apt to undergo
significant changes until a stable version has been submitted to CRAN.
You’ve been warned.

Also note this version of osfr has been temporarily renamed to *osfr2*.

``` r
# install.packages("devtools")
devtools::install_github("aaronwolen/osfr")
```

### Accessing Open Research Materials

Many researchers use OSF to archive and share their work.

``` r
library(osfr2)
#> Automatically registered OSF personal access token
cancer_proj <- osf_retrieve_node("https://osf.io/e81xl/")
```

``` r
osf_ls_nodes(cancer_proj)
#> # A tibble: 2 x 3
#>   name                                      id    meta      
#>   <chr>                                     <chr> <list>    
#> 1 Replication Studies                       p7ayb <list [3]>
#> 2 Data collection and publishing guidelines a5imq <list [3]>

osf_ls_files(cancer_proj)
#> # A tibble: 4 x 3
#>   name                                    id                     meta     
#>   <chr>                                   <chr>                  <list>   
#> 1 Adjustment of 50 studies to 37 studies… 565602398c5e4a3877d72… <list [3…
#> 2 papers_and_keywords.xlsx                553e671b8c5e4a219919e… <list [3…
#> 3 Full_dataset_of_papers_formatted.xls    553e671b8c5e4a219919e… <list [3…
#> 4 METHOD_to_select_papers.txt             553e671b8c5e4a219919e… <list [3…
```

### Creating and Populating Projects

Creating a *project* is the first step towards managing your research
with OSF. You can add folders, files, even sub-projects (called
*components*) to a project, and organize them in whatever fashion best
suits your workflow. Many of these initial setup operations can be
performed directly with
osfr.

``` r
project <- osf_create_project(title = "Gender and Political Identification")
project
#> # A tibble: 1 x 3
#>   name                                id    meta      
#>   <chr>                               <chr> <list>    
#> 1 Gender and Political Identification rcmy3 <list [3]>
```

## Function names

All osfr function names start with the prefix `osf_` to avoid namespace
clashes for common operations and improve discoverability when using
IDEs with autocomplete functionality. Functions that perform common
filesystem operations are named for their Unix counterparts. For
example, `osf_mkdir()` is used to create new directories.

`osf_*()` functions return one of the 3 `osf_tbl` subclasses based on
the 3 primary types of OSF entities: nodes, users, and files. For
operations where more than 1 entity type could be returned,
type-specific functions are provided. and named by adding a suffix based
on the OSF entity type. For example when provided an `osf_tbl_node`
representing an OSF project:

  - `osf_ls_files()` lists the files/folders
  - `osf_ls_nodes()` lists the project’s sub-components

<!-- links -->
