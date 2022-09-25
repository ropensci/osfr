
This update includes 2 fixes for uploading/downloading files to/from 'OSF'.

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

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
