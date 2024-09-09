
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coracle

<figure>
<img src="man/figures/coracle_hex.png" width="150" alt="coracle hex" />
<figcaption aria-hidden="true"><code>coracle hex</code></figcaption>
</figure>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/coracle)](https://CRAN.R-project.org/package=coracle)

<!-- badges: end -->

The goal of coracle is to provide functions for correlating data in
columns.

**CORR**elating **COL**umns -\> “corr + col” -\> `coracle`, a type of
small boat.

## Installation

You can install the development version of coracle from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Brubaker-Lab/coracle")
library(coracle)
```

This may fail for a variety of reasons. Contact Ray for assistance.

## Example

First we need some tidy data frames where observations are rows
(i.e. patients) and variables are columns (i.e. gene signatures).

``` r
index <- c("A", "B", "C", "D", "E")
up_values <- 1:5
down_values <- 5:1
random_values <- c(runif(5))


df1 <- data.frame(i = index,
                  u1 = up_values,
                  d1 = down_values,
                  r1 = random_values)
df2 <- data.frame(i = index,
                  u2 = up_values,
                  d2 = down_values,
                  r2 = random_values)

df1
#>   i u1 d1        r1
#> 1 A  1  5 0.4664959
#> 2 B  2  4 0.3959033
#> 3 C  3  3 0.9710254
#> 4 D  4  2 0.1760963
#> 5 E  5  1 0.7149128
df2
#>   i u2 d2        r2
#> 1 A  1  5 0.4664959
#> 2 B  2  4 0.3959033
#> 3 C  3  3 0.9710254
#> 4 D  4  2 0.1760963
#> 5 E  5  1 0.7149128
```

Now we’ll correlate `df1` with `df2` using `coracle`’s `corr_col`
function:

``` r
library(coracle)

result <- corr_col(x_data = df1,
                   y_data = df2,
                   x_name = "df1",
                   y_name = "df2")
#> Joining data by these columns. Provide `join_vars` to override.
#> i = i

result
#> # A tibble: 9 × 8
#>   df1   df2     rho        p     n        q    q_df1    q_df2
#>   <chr> <chr> <dbl>    <dbl> <int>    <dbl>    <dbl>    <dbl>
#> 1 u1    u2      1   3.97e-24     5 1.19e-23 1.19e-23 1.19e-23
#> 2 u1    d2     -1   1.12e-23     5 2.02e-23 1.69e-23 1.69e-23
#> 3 u1    r2      0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 4 d1    u2     -1   1.12e-23     5 2.02e-23 1.69e-23 1.69e-23
#> 5 d1    d2      1   3.97e-24     5 1.19e-23 1.19e-23 1.19e-23
#> 6 d1    r2     -0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 7 r1    u2      0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 8 r1    d2     -0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 9 r1    r2      1   3.97e-24     5 1.19e-23 1.19e-23 1.19e-23
```
