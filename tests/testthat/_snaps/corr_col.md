# expected inputs snapshots

    Code
      f(x = data.frame(up, down, random, constant, nonnumeric))
    Output
             x        y  rho            p                        message            q
      1     up     down -1.0 1.123412e-23                           <NA> 3.370237e-23
      2     up   random  0.9 3.738607e-02                           <NA> 3.738607e-02
      3     up constant   NA           NA the standard deviation is zero           NA
      4   down   random -0.9 3.738607e-02                           <NA> 3.738607e-02
      5   down constant   NA           NA the standard deviation is zero           NA
      6 random constant   NA           NA the standard deviation is zero           NA

