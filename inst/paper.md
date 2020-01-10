---
title: 'osfr: An R Interface to the Open Science Framework'
tags:
  - R
  - open science
  - reproducible research
  - project management
authors:
  - name: Aaron R. Wolen
    orcid: 0000-0003-2542-2202
    affiliation: 1
  - name: Chris H.J. Hartgerink
    orcid: 0000-0003-1050-6809
    affiliation: 2
  - name: Ryan Hafen
    affiliation: 3
  - name: Brian G. Richards
    affiliation: 4
  - name: Courtney K. Soderberg
    orcid: 0000-0003-1227-7042
    affiliation: 5
  - name: Timothy P. York
    orcid: 0000-0003-4068-4286
    affiliation: 6
affiliations:
  - name: James D. Eason Transplant Research Institute, Department of Surgery, University of Tennessee Health Science Center
    index: 1
  - name: Liberate Science GmbH
    index: 2
  - name: Department of Statistics, Purdue University
    index: 3
  - name: Merkle Group Inc.
    index: 4
  - name: Center for Open Science
    index: 5
  - name: Data Science Lab, Department of Human and Molecular Genetics, Virginia Commonwealth University
    index: 6
date: 27 November 2019
bibliography: paper.bib
---

# Background

Reproducible research requires effective project management workflows that promote consistency and facilitate transparency. Hallmarks of effective workflows include strategies for tracking the provenance of results, recording intermediate changes, conveniently documenting procedures, and working with collaborators without duplicating effort [@Sandve:2013]. For technically skilled researchers, the combination of version control software (VCS) such as git and cloud-based project repositories (e.g., GitHub and GitLab) enable highly effective workflows [@Ram:2013de] that facilitate automation and computational reproducibility. However, these tools have a steep learning curve, especially for researchers whose training is far removed from software development. Alternatively, the Open Science Framework (OSF) offers much of the same functionality through an intuitive point-and-click web-based interface, significantly lowering the barrier to adopting best practices for researchers of all skill levels, or research groups composed of individuals with different levels of computational expertise [@Sullivan:2019]. Yet the increase in accessibility comes at the cost of limiting opportunities for automation. `osfr` fills this gap for R users by allowing them to programmatically interact with OSF through a suite of functions for managing their projects and files.

# Functional Overview

On OSF, individual repositories are referred to as *projects* and serve as the top-level unit of content organization. New projects can be created with osfr using `osf_create_project()`, which allows you to specify the project's title, provide a description, and indicate whether it should be private (the default) or publicly accessible. Every OSF project includes a cloud-based storage bucket where files can be stored and organized into directories. You can use `osf_mkdir()` to add directories and `osf_upload()` to populate the project with files. osfr supports recursively uploading nested directories, making it possible to easily mirror the contents of a local project on OSF. For existing projects and project files, osfr provides functions for most common file operations such as copying (`osf_cp()`), moving (`osf_mv()`), deleting (`osf_rm()`), and downloading (`osf_download()`).

A key organizational feature of OSF is the ability to augment a project's structure with sub-projects, which are referred to as *components* on OSF, and can be added with the `osf_create_component()` function. Like top-level projects, every component is assigned a unique URL upon creation and contains its own cloud-based storage bucket, activity log, wiki, and user permissions. This allows users to create arbitrarily nested projects that can easily scale to meet the needs of even large, multi-institutional collaborations (see the [Cancer Biology Reproducibility Project][cbrp] for a great example). Whatever the scale of your work, adopting a consistent structure across projects creates predictable expectations, facilitates understanding for you and your collaborators [@Wilson:2017], and makes it easier to stay organized as a project inevitably grows in complexity over time. Maintaining a consistent structure can be a challenge, especially if implemented in an *ad hoc* process, but osfr enables you to codify your preferred organizational structure of components and directories in a simple script that can be run at the outset of every new project.

## Implementation and Design

osfr is built on the OSF public REST API, available at https://developer.osf.io, using ROpenSci's HTTP client, crul [@crul], for API Communication . In order to provide an interface that feels natural to R users, the JSON responses from the OSF API are encapsulated as R objects using the custom`osf_tbl` class. These specialized `data.frame`-like structures are depply nested and store information using a list-column approach based on [googledrive][]'s `dribble` class [@googledrive]. The vast majority of osfr functions return `osf_tbl`s as output and expect them as input, so that method chaining is possible using [magrittr][]'s pipe operator [@magrittr]. End users can safely ignore these implementation details while making use of the provided osfr functions.

Exported osfr functions all start with the prefix, `osf_`, following the `<prefix>_<verb>` convention used in packages like [stringr][] [@stringr], which facilitates auto-completion in supported IDEs (like RStudio) and avoids namespace clashes with other packages that perform similar file-based operations. Where possible, we adopt the names of common Unix utilities that perform analogous tasks (e.g., `osf_cp()`, `osf_mkdir()`).

# Summary

`osfr` provides an idiomatic R interface to OSF (Open Science Framework, https://www.osf.io), a free and open source web application that is part open-access repository and part collaborative project management tool.

# References

<!-- link -->
[cbrp]: https://osf.io/e81xl/ "Reproducibility Project: Cancer Biology"
[googledrive]: https://googledrive.tidyverse.org
[magrittr]: https://magrittr.tidyverse.org
[stringr]: https://stringr.tidyverse.org
