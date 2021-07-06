
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doMIsaul <img src='man/figures/logo.png' align="right" height="104" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/last-commit/LilithF/doMIsaul.svg)](https://github.com/LilithF/doMIsaul/commits/main)
<!-- badges: end -->

The goal of is to provide function to perform unsupervised and
semisupervised learning for an incomplete dataset.

## Installation

<!-- You can install the released version of doMIsaul from -->
<!-- [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("doMIsaul") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LilithF/doMIsaul")
```

## Example

This is a basic example which shows you how to perform unsupervised
learning for an incomplete dataset:

``` r
library(doMIsaul)
## basic example code
```

This is a basic example which shows you how to perform semisupervised
learning for an incomplete dataset with a survival outcome:

``` r
data(cancer, package = "survival")
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
