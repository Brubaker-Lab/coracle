# expected inputs snapshots

    Code
      suppressMessages(f(x = data.frame(index, up, down, random, constant, nonnumeric,
        all_na), y = data.frame(index, up, down, random, constant, nonnumeric, all_na),
      x_name = "first", y_name = "second", xy_join = join_by(index)))
    Output
      # A tibble: 25 x 8
         first second     rho         p message            q_first  q_second         q
         <chr> <chr>    <dbl>     <dbl> <chr>                <dbl>     <dbl>     <dbl>
       1 up    up         1    3.97e-24 <NA>              1.19e-23  1.19e-23  1.19e-23
       2 up    down      -1    1.12e-23 <NA>              1.69e-23  1.69e-23  2.02e-23
       3 up    random     0.9  3.74e- 2 <NA>              3.74e- 2  3.74e- 2  3.74e- 2
       4 up    constant  NA   NA        the standard de~ NA        NA        NA       
       5 up    all_na    NA   NA        not enough fini~ NA        NA        NA       
       6 down  up        -1    1.12e-23 <NA>              1.69e-23  1.69e-23  2.02e-23
       7 down  down       1    3.97e-24 <NA>              1.19e-23  1.19e-23  1.19e-23
       8 down  random    -0.9  3.74e- 2 <NA>              3.74e- 2  3.74e- 2  3.74e- 2
       9 down  constant  NA   NA        the standard de~ NA        NA        NA       
      10 down  all_na    NA   NA        not enough fini~ NA        NA        NA       
      # i 15 more rows

---

    Code
      suppressMessages(f(x = data.frame(index, up, down, random, nonnumeric)))
    Output
           x      y  rho            p message            q
      1   up   down -1.0 1.123412e-23      NA 3.370237e-23
      2   up random  0.9 3.738607e-02      NA 3.738607e-02
      3 down random -0.9 3.738607e-02      NA 3.738607e-02

