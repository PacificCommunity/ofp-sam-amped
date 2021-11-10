
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AMPLE

<!-- badges: start -->
<!-- badges: end -->

AMPLE provides three Shiny apps to explore how Harvest Control Rules
(HCRs) work and how performance indicators can be used to select between
them.

The three apps are:

-   *Introduction to HCRs* provides a simple overview to how HCRs work.
    Users are able to select their own HCR and step through its
    performance, year by year. Biological variability and estimation
    error are introduced.
-   *Measuring performance* builds on the previous app and introduces
    the idea of using performance indicators to evaluate HCR
    performance.
-   *Comparing performance* allows multiple HCRs to be created and
    tested, and their performance compared so that the preferred HCR can
    be selected.

## Installation

You can install the released version of AMPLE from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("AMPLE")
```

Alternatively, you can get the latest version from the GitHub repository
[here](https://github.com/PacificCommunity/ofp-sam-amped/tree/master/AMPLE)
by installing it with the *devtools* package.

``` r
devtools::install_github("PacificCommunity/ofp-sam-amped/tree/master/AMPLE")
```

## Usage

Load the library as usual:

``` r
library(AMPLE)
```

The apps can be launched by calling the appropriate function:

``` r
intro_hcr()
```

``` r
measuring_performance()
```

``` r
comparing_performance()
```

The apps are also hosted online at:

-   <https://ofp-sam.shinyapps.io/AMPLE-intro-hcr/>
-   <https://ofp-sam.shinyapps.io/AMPLE-measuring-performance/>
-   <https://ofp-sam.shinyapps.io/AMPLE-comparing-performance/>

## Vignettes

There are three vignettes that walk users through the app. These can be
used in workshops etc to help learning.

You can access them from R using:

``` r
vignette("intro_hcr", package="AMPLE")
vignette("measuring_performance", package="AMPLE")
vignette("comparing_performance", package="AMPLE")
```

The vignettes are also accessible through the Shiny apps, under the
‘Information’ tab.
