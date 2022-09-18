# Use brio to write text to files in tests so \n is used for line endings on
# Windows. This produces files with identical sizes on all platforms allowing
# vcr to match requests that include file sizes in the body.
library(brio)
library(testthat)
library(osfr)

test_check("osfr")
