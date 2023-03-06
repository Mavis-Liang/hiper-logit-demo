approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)) {
    x_m <- x
    x_m[i] <- x_m[i] - dx
    x_p <- x
    x_p[i] <- x_p[i] + dx
    numerical_grad[i] <- (func(x_p) - func(x_m)) / (2 * dx)
  }
  return(numerical_grad)
}


test_that("linear model's analytical gradient is close to numerical one", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  loglik_func <- function (coef) {
    gaussian_loglik(coef, data$design, data$outcome)
  }
  set.seed(615)
  n_test <- 10
  grads_are_close <- TRUE
  for (i in 1:n_test) {
    if (!grads_are_close) break
    regcoef <- rnorm(n_pred)
    analytical_grad <- linear_gradient(regcoef, data$design, data$outcome)
    numerical_grad <- approx_grad(loglik_func, regcoef)
    grads_are_close <- are_all_close(
      analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
    )
  }
  expect_true(grads_are_close)
})


test_that("logistic model's analytical gradient is close to numerical one", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome

  loglik_func <- function (coef) {
    logit_loglik(coef, data$design, data$outcome)
  }

  set.seed(615)
  n_test <- 10
  grads_are_close <- TRUE
  for (i in 1:n_test) {
    if (!grads_are_close) break
    regcoef <- rnorm(n_pred)
    analytical_grad <- logit_gradient(regcoef, data$design, data$outcome)
    numerical_grad <- approx_grad(loglik_func, regcoef)
    grads_are_close <- are_all_close(
      analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
    )
  }
  expect_true(grads_are_close)
})










