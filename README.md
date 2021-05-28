
<!-- README.md is generated from README.Rmd. Please edit that file -->

# XPolaris

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/lhmrosso/XPolaris.svg?branch=master)](https://travis-ci.com/lhmrosso/XPolaris)
<!-- badges: end -->

The goal of XPolaris is to facilitate the access to detailed soil data
at any geographical location within the United States (US). Without the
need of advanced skills on R-programming, users will be able to convert
raster data from the POLARIS database into traditional spreadsheet
format \[i.e., Comma-Separated Values (CSV)\], easy to manipulate and
process for further data analyses.

## Installation

You can install the released version of XPolaris from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("XPolaris")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lhmrosso/XPolaris")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(XPolaris)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
