
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coracle

<img src="man/figures/coracle_hex.png" width="150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/coracle)](https://CRAN.R-project.org/package=coracle)

<!-- badges: end -->

Correlations of Columns for Tidy Data \>\> “Corr” & “Col” \>\>
“Coracle”, a type of small boat.

## Installation

You can install the development version of coracle from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Brubaker-Lab/coracle")
```

This may fail for a variety of reasons.

## Examples

These functions are written to accommodate a few different cases.

### One Data Frame

When `coracle` receives a single data frame (the required `x` argument)
it does pairwise correlation between all pairs of numeric columns within
that data frame. For example:

``` r
index <- c("A", "B", "C", "D", "E")
up_values <- 1:5
down_values <- 5:1
random_values <- c(runif(5))
missing_values <- c(NA, NA, NA, NA, 1)

df <- data.frame(index, up_values, down_values, random_values, missing_values)

df
#>   index up_values down_values random_values missing_values
#> 1     A         1           5     0.2307645             NA
#> 2     B         2           4     0.2953659             NA
#> 3     C         3           3     0.5861620             NA
#> 4     D         4           2     0.1854683             NA
#> 5     E         5           1     0.4417778              1
```

Next we’ll correlate that data frame with `coracle`:

``` r
library(coracle)

result <- corr_col(x = df)

result
#>               x              y  rho            p                          error
#> 1     up_values    down_values -1.0 1.123412e-23                           <NA>
#> 2     up_values  random_values  0.2 7.470601e-01                           <NA>
#> 3     up_values missing_values   NA           NA not enough finite observations
#> 4   down_values  random_values -0.2 7.470601e-01                           <NA>
#> 5   down_values missing_values   NA           NA not enough finite observations
#> 6 random_values missing_values   NA           NA not enough finite observations
#>              q
#> 1 3.370237e-23
#> 2 7.470601e-01
#> 3           NA
#> 4 7.470601e-01
#> 5           NA
#> 6           NA
```

Notes:

- Correlations are calculated for every combination of numeric columns.
- Non numeric data (such as `index`) are excluded.
- Correlations which could not be calculated return `NA` and a brief
  `error` description.

### Two Data Frames
