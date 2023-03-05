test_that("are_all_close() returns true", {
  set.seed(3)
  x = runif(20, 0.1, 1)
  y = x - (1e-3) * x

  expect_true(are_all_close(x, y,
     abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("are_all_close() returns FALSE because the abs error is above abs_tol", {
  set.seed(3)
  x = runif(20, 10, 11)
  y = x - 0.1 * x # Above abs_tol but below rel_tol

  expect_false(are_all_close(x, y,
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("are_all_close() returns FALSE because the relative error is above rel_tol", {
  set.seed(3)
  x = c(runif(20, 0, 1e-3))
  y = x - 0.1 * x # Above rel_tol but below abs_tol

  expect_false(are_all_close(x, y,
                            abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
