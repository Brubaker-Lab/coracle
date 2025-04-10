# coracle

    Code
      f(cdo_1, cdo_2)$data
    Output
      # A tibble: 27 x 6
         cols_1 cols_2         rho          p     n message         
         <chr>  <chr>        <dbl>      <dbl> <int> <chr>           
       1 const  <NA>        NA     NA            NA Constant values.
       2 down   down         1      2.32e-189    26 <NA>            
       3 down   random      -0.163  4.36e-  1    25 <NA>            
       4 down   random_down  0.991  2.20e- 16    19 <NA>            
       5 down   random_up   -0.99   4.54e- 21    25 <NA>            
       6 down   up          -1      0            24 <NA>            
       7 random down        -0.163  4.36e-  1    25 <NA>            
       8 random random       1      0            25 <NA>            
       9 random random_down -0.344  1.63e-  1    18 <NA>            
      10 random random_up    0.142  4.97e-  1    25 <NA>            
      # i 17 more rows

---

    Code
      str(f(cdo_1, cdo_2)$data)
    Output
      tibble [27 x 6] (S3: tbl_df/tbl/data.frame)
       $ cols_1 : chr [1:27] "const" "down" "down" "down" ...
       $ cols_2 : chr [1:27] NA "down" "random" "random_down" ...
       $ rho    : num [1:27] NA 1 -0.163 0.991 -0.99 ...
       $ p      : num [1:27] NA 2.32e-189 4.36e-01 2.20e-16 4.54e-21 ...
       $ n      : int [1:27] NA 26 25 19 25 24 25 25 18 25 ...
       $ message: chr [1:27] "Constant values." NA NA NA ...

