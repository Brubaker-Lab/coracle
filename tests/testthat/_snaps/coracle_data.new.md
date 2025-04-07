# Arg `data` is a data.frame

    Code
      f$new(NULL)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "305ae71".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

---

    Code
      f$new(1)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "dcbeb75".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

---

    Code
      f$new(list(1, 2, 3))
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "176924c".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

# Arg `join` selects exactly one column

    Code
      f$new(mtcars, join = NULL, disp, mpg)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "322a5b1".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `join` must select exactly one column.

---

    Code
      f$new(mtcars, join = c(cyl, drat), disp, mpg)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "e200347".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `join` must select exactly one column.

---

    Code
      f$new(mtcars, join = abcd, disp, mpg)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "cdd7d09".
      i `version` = "1.0.2".
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

# Arg `vals` selects one or more column(s)

    Code
      f$new(mtcars, cyl, vals = NULL, mpg)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "09a5523".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `vals` requires a selection.

---

    Code
      f$new(mtcars, cyl, vals = abcd, mpg)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "2f2eede".
      i `version` = "1.0.2".
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

# Arg `grps` selects one or more column(s)

    Code
      f$new(mtcars, cyl, disp, grps = NULL)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "cfa159d".
      i `version` = "1.0.2".
    Condition
      Error in `f$new()`:
      x `grps` requires a selection.

---

    Code
      f$new(mtcars, cyl, disp, grps = abcd)
    Message
      
      -- Creating new <coracle_data> object. -----------------------------------------
      i `id` = "acf2440".
      i `version` = "1.0.2".
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

