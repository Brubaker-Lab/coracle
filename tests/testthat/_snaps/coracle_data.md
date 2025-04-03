# Arg `data` is a data.frame

    Code
      f$new(NULL)
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

---

    Code
      f$new(1)
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

---

    Code
      f$new(list(1, 2, 3))
    Condition
      Error in `f$new()`:
      x `data` requires a <data.frame>.

# Arg `join` selects exactly one column

    Code
      f$new(mtcars, join = NULL, disp, mpg)
    Condition
      Error in `f$new()`:
      x `join` must select exactly one column.

---

    Code
      f$new(mtcars, join = c(cyl, drat), disp, mpg)
    Condition
      Error in `f$new()`:
      x `join` must select exactly one column.

---

    Code
      f$new(mtcars, join = abcd, disp, mpg)
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

# Arg `vals` selects one or more column(s)

    Code
      f$new(mtcars, cyl, vals = NULL, mpg)
    Condition
      Error in `f$new()`:
      x `vals` requires a selection.

---

    Code
      f$new(mtcars, cyl, vals = abcd, mpg)
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

# Arg `grps` selects one or more column(s)

    Code
      f$new(mtcars, cyl, disp, grps = NULL)
    Condition
      Error in `f$new()`:
      x `grps` requires a selection.

---

    Code
      f$new(mtcars, cyl, disp, grps = abcd)
    Condition
      Error in `initialize()`:
      ! Can't select columns that don't exist.
      x Column `abcd` doesn't exist.

