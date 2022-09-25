
This update includes fixes for uploading/downloading files to/from 'OSF'.
This version includes a new feature and two minor improvement of the function `ncbi_snp_query`,
and a change in the vignette (we now pre-compile the vignette to avoid long runtimes).

## Test environments
* local macOS install, R 4.2.1
* win-builder (devel and release)
* Windows Server 2022, 64 bit (on R-hub), R-devel
* Ubuntu Linux 20.04.1 LTS, GCC (on R-hub), R-release
* Fedora Linux, clang, gfortran (on R-hub), R-devel

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE that on Windows Server 2022 (Rhub), R-devel:

```
❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Reverse dependencies
