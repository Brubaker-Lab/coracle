# expected outputs snapshots

    Code
      suppressMessages(f(x = data[, c("name", "mpg")], y = data[, c("name", "disp",
        "hp")], z = data[, c("name", "wt", "cyl")]))
    Output
      # A tibble: 2 x 10
        x     y     z          rho       p     n message      q    q_x     q_y
        <chr> <chr> <chr>    <dbl>   <dbl> <int> <lgl>    <dbl>  <dbl>   <dbl>
      1 mpg   disp  wt, cyl -0.206 0.275      32 NA      0.275  0.275  0.275  
      2 mpg   hp    wt, cyl -0.470 0.00876    32 NA      0.0175 0.0175 0.00876

---

    Code
      ppcor::pcor.test(x = data$mpg, y = data$disp, z = data[, c("wt", "cyl")],
      method = "spearman")
    Output
          estimate   p.value statistic  n gp   Method
      1 -0.2059711 0.2748444 -1.113778 32  2 spearman

---

    Code
      ppcor::pcor.test(x = data$mpg, y = data$hp, z = data[, c("wt", "cyl")], method = "spearman")
    Output
          estimate     p.value statistic  n gp   Method
      1 -0.4700649 0.008763914 -2.818105 32  2 spearman

---

    Code
      suppressMessages(f(x = data[, c("name", "mpg")], y = data[, c("name", "disp",
        "hp")], z = data[, c("name", "wt", "cyl")]))
    Output
      # A tibble: 2 x 10
        x     y     z          rho       p     n message      q    q_x     q_y
        <chr> <chr> <chr>    <dbl>   <dbl> <int> <lgl>    <dbl>  <dbl>   <dbl>
      1 mpg   disp  wt, cyl -0.206 0.275      32 NA      0.275  0.275  0.275  
      2 mpg   hp    wt, cyl -0.470 0.00876    32 NA      0.0175 0.0175 0.00876

---

    Code
      ppcor::pcor.test(x = data$mpg, y = data$disp, z = data[, c("wt", "cyl")],
      method = "spearman")
    Output
          estimate   p.value statistic  n gp   Method
      1 -0.2059711 0.2748444 -1.113778 32  2 spearman

---

    Code
      ppcor::pcor.test(x = data$mpg, y = data$hp, z = data[, c("wt", "cyl")], method = "spearman")
    Output
          estimate     p.value statistic  n gp   Method
      1 -0.4700649 0.008763914 -2.818105 32  2 spearman

