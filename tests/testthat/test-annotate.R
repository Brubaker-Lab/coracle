atomic <- 1
vector <- c(1:3)
dataframe <- data.frame(x = vector, y = vector, z = vector)

test_that("Annotations", {

  # Prevents "Empty Test" warning
  expect_true(exists("as_"))
  expect_true(exists("is_"))
  expect_true(exists("un_"))

  test_that("Using `*_join`", {
    # Prevents "Empty Test" warning
    expect_true(exists("as_join"))
    expect_true(exists("is_join"))
    expect_true(exists("un_join"))

    as_f <- as_join
    is_f <- is_join
    un_f <- un_join

    test_that("Atomic input", {
      input <- atomic

      expect_identical(input |> as_f() |> un_f(), input)
      expect_false(input |> is_f())
      expect_true(input |> as_f() |> is_f())

    })



    test_that("Vector input", {
      input <- vector

      expect_identical(input |> as_f() |> un_f(), input)
      expect_false(input |> is_f())
      expect_true(input |> as_f() |> is_f())

      temp <- input |> as_f()
      element <- temp[1]

      expect_false(element |> is_f())

    })

    test_that("data.frame input", {
      input <- dataframe

      expect_identical(input |> as_f() |> un_f(), input)
      expect_false(input |> is_f())
      expect_true(input |> as_f() |> is_f())

      input[[1]] <- as_f(input[[1]])

      expect_true(input[[1]] |> is_f())
      expect_equal(input |>
                     dplyr::select(where(is_f)) |>
                     names() |>
                     length(), 1)

      input[[1]] <- un_f(input[[1]])

      expect_false(input[[1]] |> is_f())
      expect_equal(input |>
                     dplyr::select(where(is_f)) |>
                     names() |>
                     length(), 0)

    })
  })

  test_that("Using `*_coracle_df`", {
    # Prevents "Empty Test" warning
    expect_true(exists("as_coracle_df"))
    expect_true(exists("is_coracle_df"))
    expect_true(exists("un_coracle_df"))

    as_f <- as_coracle_df
    is_f <- is_coracle_df
    un_f <- un_coracle_df

    test_that("Non-data.frame input", {
      input <- atomic

      expect_error(input |> as_f())

      })

    test_that("data.frame input", {

      input <- dataframe

      expect_identical(input |> as_f() |> un_f(), input)
      expect_false(input |> is_f())
      expect_true(input |> as_f() |> is_f())

    })
  })

  test_that("`is_coracle`", {
    # Prevents "Empty Test" warning
    expect_true(exists("is_coracle"))

    f <- is_coracle

    input <- c(1, 2, 3)

    expect_false(input |> f())
    expect_true(input |> as_join() |> f())

    input <- dataframe
    input[[1]] <- as_join(input[[1]])

    expect_true(input[[1]] |> f())
    expect_equal(input |>
                   dplyr::select(where(f)) |>
                   names() |>
                   length(), 1)

    input[[1]] <- un_join(input[[1]])

    expect_false(input[[1]] |> f())
    expect_equal(input |>
                   dplyr::select(where(f)) |>
                   names() |>
                   length(), 0)

  })

  test_that("`un_annotate`", {
    # Prevents "Empty Test" warning
    expect_true(exists("un_coracle"))

    f <- un_coracle

    input <- 1

    expect_equal(input |> f(), input)
    expect_identical(input |> as_join() |> f(), input)

    input <- dataframe

    input[[1]] <- as_join(input[[1]])

    expect_equal(input |>
                   dplyr::select(where(is_join)) |>
                   names() |>
                   length(), 1)
    expect_equal(input |>
                   mutate(across(where(is_join), f)) |>
                   dplyr::select(where(is_join)) |>
                   names() |>
                   length(), 0)
  })
})
