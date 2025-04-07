test_that("`%same_as%` comparisons", {

  expect_true(NA %same_as% NA)
  expect_false(NA %same_as% "A")
  expect_false(NA %same_as% 1)

  expect_true("A" %same_as% "A")
  expect_false("A" %same_as% 1)
  expect_false("A" %same_as% NA)

  expect_true(1 %same_as% 1)
  expect_false(1 %same_as% "A")
  expect_false(1 %same_as% NA)

  expect_true(!any(1:5 %same_as% NA))
  expect_true(any(c(1:5, NA) %same_as% NA))

})
