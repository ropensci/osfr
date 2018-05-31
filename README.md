# osfr: R Interface to the Open Science Framework

  [![Build Status](https://travis-ci.org/CenterForOpenScience/osfr.svg?branch=master)](https://travis-ci.org/CenterForOpenScience/osfr)[![codecov](https://codecov.io/gh/chartgerink/osfr/branch/master/graph/badge.svg)](https://codecov.io/gh/chartgerink/osfr)

The R package `osfr` interfaces with the [Open Science Framework (OSF)](https://osf.io) to both get information from the OSF and push information to the OSF. If you are looking for a Python client see [`osf-cli`](https://github.com/dib-lab/osf-cli).

To install, you currently have to have [`devtools`](https://github.com/hadley/devtools) installed, considering the package is not yet available on CRAN. When it it available on CRAN, we will celebrate here :palm_tree: Install and login is as follows from `R`:

```R
devtools::install_github('chartgerink/osfr')

login()
```

Considering this software is still very much in development, please don't use it to build anything that requires stability. Code might very well break still throughout the development process. When `v1.0.0` comes around, that's when it is reasonable to start building on this reliably (that's when it will also be on CRAN).

# Contributing

Contributions are definitely welcome! There have been some great uninvited contributions, that have made the project much better. If you're looking for ways to contribute, [the issues](https://github.com/chartgerink/osfr/issues) is always a good place to start. If you notice a typo, also please submit a PR! And most of all, this project should not only be for those that control [the ephemeral skill of using `git`](https://imgs.xkcd.com/comics/git.png) so if you want to contribute you can also always e-mail to [chris@libscie.org](mailto:chris@libscie.org).

Note that for this project there is a clear [Code of Conduct](CODE_OF_CONDUCT.md) and that in contributing to the project you are expected to keep to these guidelines. If you are contributing to the project, please also keep an eye out for problems and don't turn your back on harassment, inappropriate comments etc.
