
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osfr <a href="https://github.com/aaronwolen/osfr"><img src="man/figures/logo.png" align="right" height="139" /></a>

[![Build
Status](https://travis-ci.org/aaronwolen/osfr.svg?branch=master)](https://travis-ci.org/aaronwolen/osfr)
[![Coverage
status](https://codecov.io/gh/aaronwolen/osfr/branch/master/graph/badge.svg)](https://codecov.io/github/aaronwolen/osfr?branch=master)

## Overview

osfr provides a suite of functions for interacting with
[OSF](https://osf.io "Open Science Framework") (<https://osf.io>) that
are primarily focused on project management workflows.

**What is OSF?**

*OSF is a free and [open
source](https://github.com/CenterForOpenScience/osf.io "OSF's GitHub Repository")
project management repository designed to support researchers across
their entire project lifecycle. The service includes unlimited cloud
storage and file version history, providing a centralized location for
all your research materials that can be kept private, shared with select
collaborators, or made publicly available with citable DOIs.*

## Installation

This package is currently under development and is apt to undergo
significant changes until a stable version has been submitted to CRAN.
You’ve been warned.

Also note this version of osfr has been temporarily renamed to *osfr2*.

``` r
# install.packages("devtools")
devtools::install_github("aaronwolen/osfr")
```

## Usage Examples

### Accessing Open Research Materials

Many researchers use OSF to archive and share their work. If you come
across a paper that cites an OSF repository, osfr can be used to explore
the project and download the associated files—you only need the
project’s URL or GUID (global unique identifier) to get started.

Here we’ll retrieve the main project for the Cancer Reproducibility
Project (<https://osf.io/e81xl/>).

``` r
library(osfr2)

cr_project <- osf_retrieve_node("e81xl")
cr_project
#> # A tibble: 1 x 3
#>   name                                    id    meta      
#>   <chr>                                   <chr> <list>    
#> 1 Reproducibility Project: Cancer Biology e81xl <list [3]>
```

This returns an `osf_tbl` object with a single row representing the
retrieved project. Let’s list the files that have been uploaded to this
project.

``` r
osf_ls_files(cr_project)
#> # A tibble: 4 x 3
#>   name                                     id                     meta     
#>   <chr>                                    <chr>                  <list>   
#> 1 Adjustment of 50 studies to 37 studies.… 565602398c5e4a3877d72… <list [3…
#> 2 papers_and_keywords.xlsx                 553e671b8c5e4a219919e… <list [3…
#> 3 Full_dataset_of_papers_formatted.xls     553e671b8c5e4a219919e… <list [3…
#> 4 METHOD_to_select_papers.txt              553e671b8c5e4a219919e… <list [3…
```

This returns another `osf_tbl` with 1 row for each of the files and
directories in the project. We can examine any of these files directly
on OSF with `osf_open()`, which opens the corresponding file’s view in
your default browser.

This project contains 2 ***components***: *Replication Studies* and
*Data collection and publishing guidelines*. We can list these
components with osfr using `osf_ls_nodes()`.

``` r
osf_ls_nodes(cr_project)
#> # A tibble: 2 x 3
#>   name                                      id    meta      
#>   <chr>                                     <chr> <list>    
#> 1 Replication Studies                       p7ayb <list [3]>
#> 2 Data collection and publishing guidelines a5imq <list [3]>
```

osfr is compatible with the [pipe
operator](https://magrittr.tidyverse.org) and
[dplyr](https://dplyr.tidyverse.org), providing a powerful set of tools
for working with `osf_tbl`s. Here, we’re listing the sub-components
nested within the *Replication Studies* component, filtering for
[*Study 19*](https://osf.io/7zqxp/) and then listing the files uploaded
to that study’s component.

``` r
library(dplyr)

cr_project %>% 
  osf_ls_nodes() %>% 
  filter(name == "Replication Studies") %>% 
  osf_ls_nodes(pattern = "Study 19") %>% 
  osf_ls_files()
#> # A tibble: 6 x 3
#>   name                                    id                      meta     
#>   <chr>                                   <chr>                   <list>   
#> 1 Replication_Study_19.docx               57c9e8ed594d9001e7a240… <list [3…
#> 2 Replication_Study_19.Rmd                578e2b23594d9001f48164… <list [3…
#> 3 Replication_Study_19_track_changes.docx 581a27b76c613b02233228… <list [3…
#> 4 Replication_Study_19_track_changes_2.d… 58714d46594d9001f801f4… <list [3…
#> 5 Response_letter_Replication_Study_19.d… 58755747b83f6901ff066a… <list [3…
#> 6 Study_19_Correction_Letter.docx         5a56569125719b000ff28b… <list [3…
```

We could continue this pattern of exploration and list the files
contained within *these* projects. You can also download local copies of
files using `osf_download()`.

### Managing Projects

You can use osfr to create projects, add sub-components, create
directories, and upload files. See the *Getting Started* vignette to
learn more about building projects with osfr, but here is a quick
example in which we:

1.  Create a new project called *Motor Trend Car Road Tests*
2.  Create a sub-component called *Car Data*
3.  Create a directory named *rawdata*
4.  Upload a file (`mtcars.csv`) to the new directory
5.  Open the uploaded file on OSF

<!-- end list -->

``` r
# create an external data file
write.csv(mtcars, "mtcars.csv")

osf_create_project(title = "Motor Trend Car Road Tests") %>% 
  osf_create_component("Car Data") %>%
  osf_mkdir("rawdata") %>%
  osf_upload("mtcars.csv") %>%
  osf_open()
```

![Screenshot of the uploaded file on OSF](man/figures/screen-shot.png)

## Details on `osf_tbls`

There are 3 main types of OSF entities that osfr can work with:

1.  **nodes:** both
    [projects](http://help.osf.io/m/projects/l/481539-create-a-project "OSF: Create a Project")
    and
    [components](http://help.osf.io/m/projects/l/481998-create-components "OSF: Create a Component")
    (i.e., sub-projects) are referred to as nodes
2.  **files:** this includes both files *and* folders stored on OSF
3.  **users:** individuals with OSF accounts

osfr represents these entities within `osf_tbl`s—specialized tibbles
that provide useful information about the entities like their `name` and
unique `id` for users, and API data in the `meta` column that’s
necessary for osfr’s internal functions. Otherwise, they’re just
`data.frames` and can be manipulated using standard functions from base
R or dplyr.

## Acknowledgments

OSF is developed by the [Center for Open
Science](https://cos.io "Center for Open Science") in Charlottesville,
VA.

The original version of osfr was developed by @chartgerink and further
developed by @bgrich. The current version was developed by @aaronwolen
and is *heavily* inspired by @jennybc and @lucymcgowan’s excellent
[googledrive](https://googledrive.tidyverse.org) package. Seriously, we
borrowed a lot of great ideas from them. Other important resources
include [http testing](https://ropensci.github.io/http-testing-book/) by
Scott Chamberlain and [R Packages](http://r-pkgs.had.co.nz) by Hadley
Wickham. Development was also greatly facilitated by OSF’s excellent
[API documentation](https://developer.osf.io "OSF API Documentation").

<!-- links -->
