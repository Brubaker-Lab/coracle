
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

## Loading

After installation, load the library with:

``` r
library(coracle)
```

## Examples

The two main functions of `coracle` (`corr_col()` and `pcor_col()`) are
written to accommodate different cases depending on the inputs.

### One Data Frame (`corr_col()`)

When `corr_col()` receives a single data frame (the required `x`
argument) it does *pairwise correlation between all pairs of numeric
columns within that data frame*.

Note: `pcor_col()` does not accept a single data frame.

For example:

``` r
dfx <- data.frame(i = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))

corr_col(x = dfx)
#>        x      y  rho            p message            q
#> 1   up_x down_x -1.0 1.123412e-23      NA 3.370237e-23
#> 2   up_x rand_x  0.9 3.738607e-02      NA 3.738607e-02
#> 3 down_x rand_x -0.9 3.738607e-02      NA 3.738607e-02
```

Note:

- Correlations are calculated for every combination of numeric columns.
- Non-numeric columns (such as `index`) are excluded.
- Correlations which could not be calculated return `NA` and a brief
  explanatory `message` (relayed from `stats::cor_test()`).
- False discovery rate (FDR) q-values are automatically calculated.

### Two Data Frames (`corr_col()`)

When `corr_col()` receives two data frames (the `x` and `y` arguments)
it does *pairwise correlation between all pairs of numeric columns
between the two data frames*.

For example:

``` r
dfx <- data.frame(i = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))
dfy <- data.frame(i = as.character(1:5),
                  up_y = 1:5,
                  down_y = 5:1,
                  rand_y = runif(5))

corr_col(x = dfx, y = dfy)
#> ℹ Joining `x` and `y` by the following columns:
#> • "i" = "i"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#> # A tibble: 9 × 8
#>   x      y        rho        p message      q_x      q_y        q
#>   <chr>  <chr>  <dbl>    <dbl> <lgl>      <dbl>    <dbl>    <dbl>
#> 1 up_x   up_y     1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 2 up_x   down_y  -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 3 up_x   rand_y  -0.6 2.85e- 1 NA      2.85e- 1 4.27e- 1 3.20e- 1
#> 4 down_x up_y    -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 5 down_x down_y   1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 6 down_x rand_y   0.6 2.85e- 1 NA      2.85e- 1 4.27e- 1 3.20e- 1
#> 7 rand_x up_y     0.9 3.74e- 2 NA      5.61e- 2 3.74e- 2 5.61e- 2
#> 8 rand_x down_y  -0.9 3.74e- 2 NA      5.61e- 2 3.74e- 2 5.61e- 2
#> 9 rand_x rand_y  -0.3 6.24e- 1 NA      6.24e- 1 6.24e- 1 6.24e- 1
```

Observe:

- Every combination of a column from `x` and a column from `y` have
  their correlation calculated. No columns from `x` are correlated with
  other `x` columns. The same is true for `y`.
  - Since there are three numeric columns in each data frame above,
    there are 9 total combinations in the result.
- There are two additional FDR `q` columns: `q_x` and `q_y` (the default
  names). These are calculated *per grouping* of the `x` and `y` values,
  respectively. If `x` or `y` are renamed with `x_name` or `y_name`
  arguments, the `q_*` columns will be renamed to match.

### Three Data Frames (`pcor_col()`)

When `pcor_col()` receives three data frames (the `x`, `y`, and `z`
arguments) it does *pairwise partial correlation between all pairs of
numeric columns between `x` and `y` using all numeric columns of `z` as
covariates*.

For example:

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

dfx <- data.frame(i = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))
dfy <- data.frame(i = as.character(1:5),
                  up_y = 1:5,
                  down_y = 5:1,
                  rand_y = runif(5))
dfz <- data.frame(i = as.character(1:5),
                  rand_z = runif(5))

pcor_col(x = dfx,
         y = dfy,
         z = dfz)
#> ℹ Joining `x` and `y` by the following columns:
#> • "i" = "i"
#> ! Override by providing `xy_join` argument.
#> ℹ Joining `x` and `z` by the following columns:
#> • "i" = "i"
#> ! Override by providing `xz_join` argument.
#> → Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#> # A tibble: 9 × 9
#>   x      y      z         rho      p message     q    q_x   q_y
#>   <chr>  <chr>  <chr>   <dbl>  <dbl> <lgl>   <dbl>  <dbl> <dbl>
#> 1 up_x   up_y   rand_z -1     0      NA      0     0      0    
#> 2 up_x   down_y rand_z  1     0      NA      0     0      0    
#> 3 up_x   rand_y rand_z  0.920 0.0799 NA      0.120 0.0799 0.120
#> 4 down_x up_y   rand_z  1     0      NA      0     0      0    
#> 5 down_x down_y rand_z -1     0      NA      0     0      0    
#> 6 down_x rand_y rand_z -0.920 0.0799 NA      0.120 0.0799 0.120
#> 7 rand_x up_y   rand_z  0.813 0.187  NA      0.210 0.280  0.187
#> 8 rand_x down_y rand_z -0.813 0.187  NA      0.210 0.280  0.187
#> 9 rand_x rand_y rand_z  0.687 0.313  NA      0.313 0.313  0.313
```

### Naming Outputs

All `coracle` functions include `*_name` arguments to override the
default column names (“x”, “y”, and “z”) in the output.

For example:

``` r

dfx <- data.frame(i = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))
dfy <- data.frame(i = as.character(1:5),
                  up_y = 1:5,
                  down_y = 5:1,
                  rand_y = runif(5))

corr_col(
  x = dfx,
  y = dfy,
  x_name = "FIRST",
  y_name = "SECOND"
)
#> ℹ Joining `x` and `y` by the following columns:
#> • "i" = "i"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#> # A tibble: 9 × 8
#>   FIRST  SECOND   rho        p message  q_FIRST q_SECOND        q
#>   <chr>  <chr>  <dbl>    <dbl> <lgl>      <dbl>    <dbl>    <dbl>
#> 1 up_x   up_y     1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 2 up_x   down_y  -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 3 up_x   rand_y   0.9 3.74e- 2 NA      3.74e- 2 5.61e- 2 5.61e- 2
#> 4 down_x up_y    -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 5 down_x down_y   1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 6 down_x rand_y  -0.9 3.74e- 2 NA      3.74e- 2 5.61e- 2 5.61e- 2
#> 7 rand_x up_y    -0.1 8.73e- 1 NA      8.73e- 1 8.73e- 1 8.73e- 1
#> 8 rand_x down_y   0.1 8.73e- 1 NA      8.73e- 1 8.73e- 1 8.73e- 1
#> 9 rand_x rand_y  -0.3 6.24e- 1 NA      8.73e- 1 6.24e- 1 8.02e- 1
```

Observe:

- The `q_*` columns of the output have also been renamed.

### Specifying Joins

By default, all `coracle` functions which accept two or more data frames
as input attempt to join the data frames by columns of the same name
found in each data frame.

For example:

``` r
dfx <- data.frame(i = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))
dfy <- data.frame(i = as.character(1:5),
                  up_y = 1:5,
                  down_y = 5:1,
                  rand_y = runif(5))

corr_col(x = dfx, y = dfy)
#> ℹ Joining `x` and `y` by the following columns:
#> • "i" = "i"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#> # A tibble: 9 × 8
#>   x      y        rho        p message      q_x      q_y        q
#>   <chr>  <chr>  <dbl>    <dbl> <lgl>      <dbl>    <dbl>    <dbl>
#> 1 up_x   up_y     1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 2 up_x   down_y  -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 3 up_x   rand_y  -0.4 5.05e- 1 NA      5.05e- 1 5.05e- 1 5.05e- 1
#> 4 down_x up_y    -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 5 down_x down_y   1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 6 down_x rand_y   0.4 5.05e- 1 NA      5.05e- 1 5.05e- 1 5.05e- 1
#> 7 rand_x up_y     0.4 5.05e- 1 NA      5.05e- 1 5.05e- 1 5.05e- 1
#> 8 rand_x down_y  -0.4 5.05e- 1 NA      5.05e- 1 5.05e- 1 5.05e- 1
#> 9 rand_x rand_y   0.5 3.91e- 1 NA      5.05e- 1 5.05e- 1 5.05e- 1
```

Observe:

- The function provides a message reporting on how the columns will be
  joined.

In some scenarios, this default behavior is not desirable. For example:

- There may be one or more columns between the two data frames which
  share a name but would be inappropriate to use for joining.
- There are no columns between the two data frames which share names.

To override this default behavior, provide a [`dplyr::join_by()` join
specification](https://dplyr.tidyverse.org/reference/join_by.html) to
the appropriate `xy_join` and/or `xz_join` argument.

For example:

``` r

library(dplyr)

dfx <- data.frame(i_x = as.character(1:5),
                  up_x = 1:5,
                  down_x = 5:1,
                  rand_x = runif(5))
dfy <- data.frame(i_y = as.character(1:5),
                  up_y = 1:5,
                  down_y = 5:1,
                  rand_y = runif(5))

corr_col(x = dfx,
         y = dfy,
         xy_join = join_by(i_x == i_y))
#> ℹ Joining `x` and `y` by the following columns:
#> • "i_x" = "i_y"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#> # A tibble: 9 × 8
#>   x      y        rho        p message      q_x      q_y        q
#>   <chr>  <chr>  <dbl>    <dbl> <lgl>      <dbl>    <dbl>    <dbl>
#> 1 up_x   up_y     1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 2 up_x   down_y  -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 3 up_x   rand_y   0.5 3.91e- 1 NA      3.91e- 1 5.87e- 1 5.87e- 1
#> 4 down_x up_y    -1   1.12e-23 NA      1.69e-23 1.69e-23 2.53e-23
#> 5 down_x down_y   1   3.97e-24 NA      1.19e-23 1.19e-23 1.79e-23
#> 6 down_x rand_y  -0.5 3.91e- 1 NA      3.91e- 1 5.87e- 1 5.87e- 1
#> 7 rand_x up_y     0.1 8.73e- 1 NA      8.73e- 1 8.73e- 1 8.73e- 1
#> 8 rand_x down_y  -0.1 8.73e- 1 NA      8.73e- 1 8.73e- 1 8.73e- 1
#> 9 rand_x rand_y  -0.2 7.47e- 1 NA      8.73e- 1 7.47e- 1 8.73e- 1
```

Observe:

- Columns used to join data frames are excluded from the calculations.
  - Since we joined by the “up” columns of each data frame (`u1` and
    `u2`), only the “down” (`d1`, `d2`) and “random” (`r1`, `r2`)
    columns are included in the result, for a total of 4 combinations.
- The `xy_join` argument expects the output of `dplyr::join_by()` which
  can be assigned to another variable which is passed to the function or
  done directly in the `coracle` function call (as shown).
