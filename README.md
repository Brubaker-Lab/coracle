
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
#> 1     A         1           5     0.1333570             NA
#> 2     B         2           4     0.4439452             NA
#> 3     C         3           3     0.7340028             NA
#> 4     D         4           2     0.1761125             NA
#> 5     E         5           1     0.3495841              1
```

Next we’ll correlate that data frame with `coracle`:

``` r
library(coracle)

result <- corr_col(x = df)

result
#>               x              y  rho            p                        message
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
- Non-numeric columns (such as `index`) are excluded.
- Correlations which could not be calculated return `NA` and a brief
  `message`.

### Two Data Frames

``` r
index <- c("A", "B", "C", "D", "E")
up_values <- 1:5
down_values <- 5:1
random_values <- c(runif(5))
missing_values <- c(NA, NA, NA, NA, 1)

df1 <- data.frame(index,
                 u1 = up_values,
                 d1 = down_values,
                 r1 = random_values)
df2 <- data.frame(index,
                 u2 = up_values,
                 d2 = down_values,
                 r2 = random_values)

df1
#>   index u1 d1        r1
#> 1     A  1  5 0.8497747
#> 2     B  2  4 0.8414915
#> 3     C  3  3 0.4322600
#> 4     D  4  2 0.6452844
#> 5     E  5  1 0.4603964
df2
#>   index u2 d2        r2
#> 1     A  1  5 0.8497747
#> 2     B  2  4 0.8414915
#> 3     C  3  3 0.4322600
#> 4     D  4  2 0.6452844
#> 5     E  5  1 0.4603964
```

``` r
library(coracle)

result <- corr_col(x = df1, y = df2)
#> ℹ Joining `x` and `y` by the following columns:
#> • "index" = "index"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).

result
#>    x  y  rho            p message            q          q_x          q_y
#> 1 u1 u2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 2 u1 d2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 3 u1 r2 -0.7 1.881204e-01      NA 1.881204e-01 1.881204e-01 1.881204e-01
#> 4 d1 u2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 5 d1 d2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 6 d1 r2  0.7 1.881204e-01      NA 1.881204e-01 1.881204e-01 1.881204e-01
#> 7 r1 u2 -0.7 1.881204e-01      NA 1.881204e-01 1.881204e-01 1.881204e-01
#> 8 r1 d2  0.7 1.881204e-01      NA 1.881204e-01 1.881204e-01 1.881204e-01
#> 9 r1 r2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
```

Notes:

- Every combination of a column from `x` and a column from `y` have
  their correlation calculated. No columns from `x` are correlated with
  other `x` columns and the same is true for `y`.
- There are two additional `q` columns, `q_x` and `q_y` (the defaults),
  which are calculated per group of the `x` and `y` values respectively.
  If `x` or `y` are renamed with `x_name` or `y_name`, respectively, the
  `q_*` columns will be renamed to match.
