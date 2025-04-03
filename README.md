
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

## 1.X.X

This package facilitates correlation of data through the following:

- Data handling, tree data structures, and pass-by-reference with `R6`.
- Parallelization via `furrr` and `future`

Data must be loaded into `coracle_data` objects with
`coracle_data$new()` *before* it can be passed into `coracle()`. The
`coracle_data` object initialization annotates, validates, and organizes
the data for correlation. It requires the user to identify which
column(s) of data to use for grouping, joining, and correlation.

### Preparing data with `coracle_data`

#### Creating a `coracle_data` object

A `coracle_data` object begins with a `data.frame`:

``` r
library(tidyverse) 
#> Warning: package 'purrr' was built under R version 4.4.2
#> Warning: package 'lubridate' was built under R version 4.4.2
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

df <- expand_grid(first_col = LETTERS[1:3],
                  second_col = letters[1:10]) %>%
  mutate(third_col = runif(nrow(.)))

df
#> # A tibble: 30 × 3
#>    first_col second_col third_col
#>    <chr>     <chr>          <dbl>
#>  1 A         a              0.482
#>  2 A         b              0.773
#>  3 A         c              0.471
#>  4 A         d              0.421
#>  5 A         e              0.529
#>  6 A         f              0.675
#>  7 A         g              0.895
#>  8 A         h              0.941
#>  9 A         i              0.768
#> 10 A         j              0.937
#> # ℹ 20 more rows
```

During initialization the user will indicate:

- `grps` - The “grouping column(s)” which identify the subject of
  interest (i.e. microbes, drugs)
- `join` - The “joining column” which will be used to join different
  datasets together for correlation (i.e. genes).
- `vals` - The “values column(s)” which contains the values for
  correlation.

``` r

cdo <- coracle_data$new(
  data = df,
  # Since `coracle_data` implements `tidyselect`, column(s) can be selected....
  # ... with unquoted names...
  grps = first_col,
  # ... with character vectors...
  join = "second_col", 
  # ... with tidyselect helpers...
  vals = starts_with("third")
  # ... and much more. Refer to `tidyselect` for details.
)

cdo # This is the `coracle_data` object
#> <coracle_data>
#>   Public:
#>     children: list
#>     chunk: NULL
#>     chunk_flags: NULL
#>     chunks: active binding
#>     clone: function (deep = FALSE) 
#>     corr_data: active binding
#>     corr_grps: active binding
#>     corr_join: active binding
#>     corr_vals: active binding
#>     data: active binding
#>     grps_cols: first_col
#>     grps_vals: NULL
#>     id: 65dbb7c
#>     initialize: function (data = NULL, grps = NULL, join = NULL, vals = NULL, 
#>     join_col: second_col
#>     join_vals: a b c d e f g h i j
#>     leaves: active binding
#>     leaves_invalid: active binding
#>     leaves_valid: active binding
#>     other_cols: 
#>     vals_col: third_col
#>     version: 1.0.0
```

#### Basics of `coracle_data` objects

`coracle_data` objects store data in a tree structure based on the
grouping column(s). The object the initialization creates is the “root
node” of a tree. It doesn’t actually contain any data!

``` r
cdo$chunk # A "chunk" is the stored unit of data in a node
#> NULL
```

Instead, the root node contains a list of “child nodes”. In this
example, there are three children corresponding to the values in the
data used for grouping (“A”, “B”, and “C”).

``` r
names(cdo$children)
#> [1] "A" "B" "C"
```

Each child node is itself a `coracle_data` object, *almost* identical to
it’s parent. Let’s take a closer look at one:

``` r
cdo$children$A
#> <coracle_data>
#>   Public:
#>     children: NULL
#>     chunk: tbl_df, tbl, data.frame
#>     chunk_flags: NULL
#>     chunks: active binding
#>     clone: function (deep = FALSE) 
#>     corr_data: active binding
#>     corr_grps: active binding
#>     corr_join: active binding
#>     corr_vals: active binding
#>     data: active binding
#>     grps_cols: first_col
#>     grps_vals: list
#>     id: 0e77864
#>     initialize: function (data = NULL, grps = NULL, join = NULL, vals = NULL, 
#>     join_col: second_col
#>     join_vals: a b c d e f g h i j
#>     leaves: active binding
#>     leaves_invalid: active binding
#>     leaves_valid: active binding
#>     other_cols: 
#>     vals_col: third_col
#>     version: 1.0.0
```

Can you spot the differences?

Unlike the root node, in this case the children do contain data!

Data is only stored in “leaf nodes” - the nodes at the extremities of
the tree - in discrete “chunks”.

If we look at all three children and their chunks we can see the entire
original data frame has been split in three based on the value in
“first_col”:

``` r
cdo$children$A$chunk
#> # A tibble: 10 × 3
#>    first_col second_col third_col
#>    <chr>     <chr>          <dbl>
#>  1 A         a              0.482
#>  2 A         b              0.773
#>  3 A         c              0.471
#>  4 A         d              0.421
#>  5 A         e              0.529
#>  6 A         f              0.675
#>  7 A         g              0.895
#>  8 A         h              0.941
#>  9 A         i              0.768
#> 10 A         j              0.937

cdo$children$B$chunk
#> # A tibble: 10 × 3
#>    first_col second_col third_col
#>    <chr>     <chr>          <dbl>
#>  1 B         a            0.0512 
#>  2 B         b            0.00336
#>  3 B         c            0.257  
#>  4 B         d            0.334  
#>  5 B         e            0.0547 
#>  6 B         f            0.311  
#>  7 B         g            0.169  
#>  8 B         h            0.930  
#>  9 B         i            0.133  
#> 10 B         j            0.530

cdo$children$C$chunk
#> # A tibble: 10 × 3
#>    first_col second_col third_col
#>    <chr>     <chr>          <dbl>
#>  1 C         a             0.825 
#>  2 C         b             0.528 
#>  3 C         c             0.0993
#>  4 C         d             0.609 
#>  5 C         e             0.663 
#>  6 C         f             0.416 
#>  7 C         g             0.104 
#>  8 C         h             0.298 
#>  9 C         i             0.306 
#> 10 C         j             0.212
```

We can actually retrieve the entire, original data frame using `$data`.
This is how you pull data out of a `coracle_data` object.

Note: Initialization may involve transformation and renaming. When you
use `$data` it will reflect those changes. All the data will be there,
but not necessarily in the same format as the original.

That note doesn’t apply to this example, but it’s an important point to
keep in mind.

``` r

output <- cdo$data

output
#> # A tibble: 30 × 3
#>    first_col second_col third_col
#>    <chr>     <chr>          <dbl>
#>  1 A         a              0.482
#>  2 A         b              0.773
#>  3 A         c              0.471
#>  4 A         d              0.421
#>  5 A         e              0.529
#>  6 A         f              0.675
#>  7 A         g              0.895
#>  8 A         h              0.941
#>  9 A         i              0.768
#> 10 A         j              0.937
#> # ℹ 20 more rows
```

The `coracle_data` object also contains other useful information:

``` r
cdo$version # The version of `coracle` used to generate the object
#> [1] "1.0.0"

cdo$id # A hashed id to distinguish between objects
#> [1] "65dbb7c"

cdo$grps_cols # List of grouping column(s)
#> [1] "first_col"

cdo$join_col # The joining column
#> [1] "second_col"

cdo$join_vals # The values found in the joining column
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"

cdo$vals_col # The values column
#> [1] "third_col"
```

That’s enough to start with.

### Correlating with `coracle`

Preparing the `coracle_data` objects for `coracle` is the hardest part.
Once the data is ready, it’s a simple matter to call the function.

Let’s create a second `coracle_data` object, this one using “X”, “Y”,
and “Z”:

``` r
df2 <- expand_grid(df2_first = LETTERS[24:26],
                  df2_second = letters[1:10]) %>%
  mutate(df2_third = runif(nrow(.)))

df2
#> # A tibble: 30 × 3
#>    df2_first df2_second df2_third
#>    <chr>     <chr>          <dbl>
#>  1 X         a            0.336  
#>  2 X         b            0.232  
#>  3 X         c            0.972  
#>  4 X         d            0.431  
#>  5 X         e            0.795  
#>  6 X         f            0.799  
#>  7 X         g            0.111  
#>  8 X         h            0.120  
#>  9 X         i            0.700  
#> 10 X         j            0.00691
#> # ℹ 20 more rows

cdo2 <- coracle_data$new(data = df2,
                         grps = 1,
                         join = 2,
                         vals = 3)

cdo2
#> <coracle_data>
#>   Public:
#>     children: list
#>     chunk: NULL
#>     chunk_flags: NULL
#>     chunks: active binding
#>     clone: function (deep = FALSE) 
#>     corr_data: active binding
#>     corr_grps: active binding
#>     corr_join: active binding
#>     corr_vals: active binding
#>     data: active binding
#>     grps_cols: df2_first
#>     grps_vals: NULL
#>     id: 490a009
#>     initialize: function (data = NULL, grps = NULL, join = NULL, vals = NULL, 
#>     join_col: df2_second
#>     join_vals: a b c d e f g h i j
#>     leaves: active binding
#>     leaves_invalid: active binding
#>     leaves_valid: active binding
#>     other_cols: 
#>     vals_col: df2_third
#>     version: 1.0.0
```

Now that we have two `coracle_data` objects, we can put them into
`coracle` to do pairwise correlation and we’ll get another
`coracle_data` object in response. We can retrieve the data with
`$data`.

``` r

cdo_output <- coracle(cdo, cdo2)

cdo_output$data
#>   first_col df2_first          rho          p  n message
#> 1         A         X -0.721212121 0.01857316 10    <NA>
#> 2         A         Y -0.418181818 0.22911284 10    <NA>
#> 3         A         Z -0.030303030 0.93377296 10    <NA>
#> 4         B         X -0.200000000 0.57958400 10    <NA>
#> 5         B         Y -0.030303030 0.93377296 10    <NA>
#> 6         B         Z  0.006060606 0.98674291 10    <NA>
#> 7         C         X  0.163636364 0.65147734 10    <NA>
#> 8         C         Y  0.090909091 0.80277173 10    <NA>
#> 9         C         Z  0.490909091 0.14965567 10    <NA>
```
