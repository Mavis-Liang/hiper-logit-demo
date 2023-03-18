test_that("Newton's QR and default solver outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_qr_out <- hiper_glm(
    design, outcome, model = 'logit')
  via_LU_out <- hiper_glm(
    design, outcome, model = 'logit', option = list(newton_solver = 'LU'))
  expect_true(are_all_close(
    coef(via_qr_out), coef(via_LU_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
