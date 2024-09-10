
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

This may fail for a variety of reasons. If it does fail, try following
[these instructions for installing R packages and setting up GitHub
credentials on the
HPC](https://github.com/Brubaker-Lab/Library/blob/82c1c998db482f1bbae00c23f0841e6ce2cb941b/HPC/Installing%20R%20Packages%2C%20Setting%20Up%20GitHub%20Credentials.md).
Contact Ray for assistance.

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
#>   i u1 d1         r1
#> 1 A  1  5 0.51361999
#> 2 B  2  4 0.20015764
#> 3 C  3  3 0.03174676
#> 4 D  4  2 0.24085009
#> 5 E  5  1 0.42413124
df2
#>   i u2 d2         r2
#> 1 A  1  5 0.51361999
#> 2 B  2  4 0.20015764
#> 3 C  3  3 0.03174676
#> 4 D  4  2 0.24085009
#> 5 E  5  1 0.42413124
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
#> 3 u1    r2     -0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 4 d1    u2     -1   1.12e-23     5 2.02e-23 1.69e-23 1.69e-23
#> 5 d1    d2      1   3.97e-24     5 1.19e-23 1.19e-23 1.19e-23
#> 6 d1    r2      0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 7 r1    u2     -0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 8 r1    d2      0.1 8.73e- 1     5 8.73e- 1 8.73e- 1 8.73e- 1
#> 9 r1    r2      1   3.97e-24     5 1.19e-23 1.19e-23 1.19e-23
```
