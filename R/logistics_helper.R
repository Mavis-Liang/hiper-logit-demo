expit <- function(x){
  1 / (1 + exp(-x))
}

## log-likelihood
logit_loglik <- function(
    reg_coef, design, outcome
) {
  if (is.list(outcome)) {
    n_success <- outcome$n_success
    n_trial <- outcome$n_trial
  } else {
    n_success <- outcome
    n_trial <- rep(1, length(n_success)) # Assume binary outcome
  }
  logit_prob <- design %*% reg_coef
  loglik <- sum(n_success * logit_prob - n_trial * log(1 + exp(logit_prob)))
  # TODO: improve numerical stability for logit_prob >> 1
  return(loglik)
}


logit_gradient <- function(reg_coef, design, outcome){
  if (is.list(outcome)) {
    n_success <- outcome$n_success
    n_trial <- outcome$n_trial
  } else {
    n_success <- outcome
    n_trial <- rep(1, length(n_success)) # Assume binary outcome
  }
  logit_prob <- design %*% reg_coef
  predicted_prob <- 1 / (1 + exp(-logit_prob))
  grad <- t(design) %*% (n_success - n_trial * predicted_prob)
  grad <- as.vector(grad)
  return(grad)
}

## BFGS
BFGS_finder_logit <- function(design, outcome, method){
  init_coef <- rep(0, ncol(design))

  obj_fn <- function(coef) {
    logit_loglik(coef, design, outcome)
  }

  obj_grad <- function(coef){
    logit_gradient(coef, design, outcome)
  }

  BFGS <- stats::optim(init_coef,
                       obj_fn, obj_grad,
                       method = method,
                       control = list(fnscale = -1))
  return(BFGS$par)
}


## Hessian matrix
hessian <- function(reg_coef, design, outcome){
  if (is.list(outcome)) {
    n_success <- outcome$n_success
    n_trial <- outcome$n_trial
  } else {
    n_success <- outcome
    n_trial <- rep(1, length(n_success)) # Assume binary outcome
  }
  logit_prob <- as.vector(design %*% reg_coef)
  predicted_prob <- 1 / (1 + exp(-logit_prob))
  weight <- n_trial * predicted_prob * (1 - predicted_prob)
  hess <- - t(design) %*% (outer(weight, rep(1, ncol(design))) * design)
  return(hess)
}




## Newton's method main function
newton <- function(X, Y, maxIt = 100, rel_tol = 1e-6, abs_tol = 1e-6){
  p <- ncol(X)
  coefs <- rep(0, p)
  loglik_curr <- logit_loglik(coefs, X, Y)
  for (i in 1:maxIt) {
    loglik_prev <- loglik_curr

    ## Update the coefficients
    gr <- as.matrix(logit_gradient(coefs, X, Y))
    h <- hessian(coefs, X, Y)
    coefs <- as.vector(coefs - solve(h, gr))

    ## Check convergence
    loglik_curr <- logit_loglik(coefs, X, Y)
    converged <- 2 * abs(loglik_curr - loglik_prev) < (abs_tol + rel_tol * abs(loglik_curr))
    if (converged){
      return(coefs)
    }
  }
  return("Newton's method not converging")
}
