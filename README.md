
<!-- README.md is generated from README.Rmd. Please edit that file -->

# XPolaris

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/lhmrosso/XPolaris.svg?branch=master)](https://travis-ci.com/lhmrosso/XPolaris)
<!-- badges: end -->

<img src="man/figures/xpolaris.png" height="300" align="right"/>

The *XPolaris* package aims to facilitate the access to detailed soil
data at any geographical location within the United States (US). The
[**POLARIS**](http://hydrology.cee.duke.edu/POLARIS/) database comprises
a 30-meter probabilistic soil series map of the contiguous United States
(US). It represents an optimization of the Soil Survey Geographic
(SSURGO) database, circumventing issues of spatial disaggregation,
harmonizing, and filling spatial gaps \[1, 2\]. Without the need of
advanced skills on R-programming, users will be able to convert raster
data into traditional spreadsheet format for further data analysis.

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

After loading the package, users must create a `data.frame` object
containing three columns (`ID`, `lat`, and `long`). Locations IDs must
be unique alphanumerical identifiers, and latitude and longitude
coordinates must be supplied as decimal degrees. The package comes with
example locations in the Kansas State, assigned to the `exkansas`
object.

``` r
library(XPolaris)
print(exkansas)
#>           ID     lat     long
#> 1    Scandia 39.8291 -97.8458
#> 2 Belleville 39.8158 -97.6720
#> 3     Ottawa 38.5398 -95.2446
```

The package is composed by three R functions:  
1) `xplot`: generates a map displaying the rater images from which
locations will be retrieved;  
2) `ximages`: downloads the images from the POLARIS database to the
user’s local machine;  
3) `xsoil`: extracts the soil data from raster images and creater a
`data.frame` object.

``` r
# Generates plot
xplot(exkansas)

# Downloading POLARIS images
df_ximages <- ximages(exkansas)

# Retrieving raster soil data
df_xsoil <- xsoil(df_ximages)
```

## References

<div id="refs" class="references csl-bib-body">

<div id="ref-Chan16" class="csl-entry">

1\. Chaney NW, Wood EF, McBratney AB, Hempel JW, Nauman TW, Brungard CW,
et al. POLARIS: A 30-meter probabilistic soil series map of the
contiguous united states. Geoderma. 2016;274:54–67.
doi:<https://doi.org/10.1016/j.geoderma.2016.03.025>.

</div>

<div id="ref-Chan19" class="csl-entry">

2\. Chaney NW, Minasny B, Herman JD, Nauman TW, Brungard CW, Morgan CLS,
et al. POLARIS soil properties: 30-m probabilistic maps of soil
properties over the contiguous united states. Water Resources Research.
2019;55:2916–38. doi:<https://doi.org/10.1029/2018WR022797>.

</div>

</div>
