
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

### One Data Frame (`x`)

When `corr_col()` receives a single data frame (the required `x`
argument) it does *pairwise correlation between all pairs of numeric
columns within that data frame*.

Note: `pcor_col()` for one data frame has not been implemented yet.

For example:

``` r
index <- c("A", "B", "C", "D", "E")
up_values <- 1:5
down_values <- 5:1
random_values <- c(runif(5))
missing_values <- c(NA, NA, NA, NA, 1)

df <- data.frame(index, up_values, down_values, random_values, missing_values)

df
#>   index up_values down_values random_values missing_values
#> 1     A         1           5    0.62974843             NA
#> 2     B         2           4    0.03040778             NA
#> 3     C         3           3    0.88239923             NA
#> 4     D         4           2    0.29420164             NA
#> 5     E         5           1    0.06058322              1

corr_col(x = df)
#>               x              y  rho            p                        message
#> 1     up_values    down_values -1.0 1.123412e-23                           <NA>
#> 2     up_values  random_values -0.2 7.470601e-01                           <NA>
#> 3     up_values missing_values   NA           NA not enough finite observations
#> 4   down_values  random_values  0.2 7.470601e-01                           <NA>
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

Note:

- Correlations are calculated for every combination of numeric columns.
- Non-numeric columns (such as `index`) are excluded.
- Correlations which could not be calculated return `NA` and a brief
  explanatory `message` (relayed from `stats::cor_test()`).
- False discovery rate (FDR) q-values are automatically calculated.

### Two Data Frames (`x`, `y`)

When `corr_col()` receives two data frames (the `x` and `y` arguments)
it does *pairwise correlation between all pairs of numeric columns
between the two data frames*.

Note: `pcor_col()` does not accept two data frames. It requires either
one or three data frames as input.

For example:

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
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719
df2
#>   index u2 d2        r2
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719

corr_col(x = df1, y = df2)
#> ℹ Joining `x` and `y` by the following columns:
#> • "index" = "index"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#>    x  y  rho            p message            q          q_x          q_y
#> 1 u1 u2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 2 u1 d2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 3 u1 r2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 4 d1 u2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 5 d1 d2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 6 d1 r2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 7 r1 u2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 8 r1 d2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 9 r1 r2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
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

### Three Data Frames (`x`,`y`,`z`)

Note: `pcor_col()` for three data frames has not been implemented yet.

Note: `corr_col()` does not accept two data frames. It requires either
one or two data frames as input.

### Naming Outputs

All `coracle` functions include `*_name` arguments to override the
default column names (“x”, “y”, and “z”) in the output.

For example:

``` r
df1
#>   index u1 d1        r1
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719
df2
#>   index u2 d2        r2
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719

corr_col(
  x = df1,
  y = df2,
  x_name = "FIRST",
  y_name = "SECOND"
)
#> ℹ Joining `x` and `y` by the following columns:
#> • "index" = "index"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#>   FIRST SECOND  rho            p message            q      q_FIRST     q_SECOND
#> 1    u1     u2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 2    u1     d2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 3    u1     r2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 4    d1     u2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 5    d1     d2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 6    d1     r2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 7    r1     u2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 8    r1     d2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 9    r1     r2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
```

Observe:

- The `q_*` columns of the output have also been renamed.

### Specifying Joins

By default, all `coracle` functions which accept two or more data frames
as input attempt to join the data frames by columns of the same name
found in each data frame.

For example:

``` r
df1
#>   index u1 d1        r1
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719
df2
#>   index u2 d2        r2
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719

corr_col(x = df1, y = df2)
#> ℹ Joining `x` and `y` by the following columns:
#> • "index" = "index"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#>    x  y  rho            p message            q          q_x          q_y
#> 1 u1 u2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 2 u1 d2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 3 u1 r2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 4 d1 u2 -1.0 1.123412e-23      NA 2.022142e-23 1.685119e-23 1.685119e-23
#> 5 d1 d2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
#> 6 d1 r2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 7 r1 u2 -0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 8 r1 d2  0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 9 r1 r2  1.0 3.971862e-24      NA 1.191559e-23 1.191559e-23 1.191559e-23
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
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

df1
#>   index u1 d1        r1
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719
df2
#>   index u2 d2        r2
#> 1     A  1  5 0.8742955
#> 2     B  2  4 0.9522968
#> 3     C  3  3 0.1981534
#> 4     D  4  2 0.9092349
#> 5     E  5  1 0.7941719

corr_col(x = df1,
         y = df2,
         xy_join = join_by(u1 == u2))
#> ℹ Joining `x` and `y` by the following columns:
#> • "u1" = "u2"
#> ! Override by providing `xy_join` argument.
#> ℹ Refer to documentation for the `by` argument of dplyr mutating joins (<https://dplyr.tidyverse.org/reference/mutate-joins.html>).
#>    x  y rho            p message            q          q_x          q_y
#> 1 d1 d2 1.0 3.971862e-24      NA 7.943725e-24 7.943725e-24 7.943725e-24
#> 2 d1 r2 0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 3 r1 d2 0.3 6.238377e-01      NA 6.238377e-01 6.238377e-01 6.238377e-01
#> 4 r1 r2 1.0 3.971862e-24      NA 7.943725e-24 7.943725e-24 7.943725e-24
```

Observe:

- Columns used to join data frames are excluded from the calculations.
  - Since we joined by the “up” columns of each data frame (`u1` and
    `u2`), only the “down” (`d1`, `d2`) and “random” (`r1`, `r2`)
    columns are included in the result, for a total of 4 combinations.
- The `xy_join` argument expects the output of `dplyr::join_by()` which
  can be assigned to another variable which is passed to the function or
  done directly in the `coracle` function call (as shown).
