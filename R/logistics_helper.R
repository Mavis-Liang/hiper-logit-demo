## log-likelihood
logit_loglik <- function(beta, X, Y){
  p <- 1 / (1 + exp(-X %*% beta))
  l <- -(t(Y) %*% log(p) + t(1 - Y) %*% log(1 - p))
  return(l)
}


## gradient
logit_gradient <- function(beta, X, Y){
  y_est <- 1 / (1 + exp(-X %*% beta))
  gr = as.vector(t(X) %*% (y_est - Y))
  return(gr)
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
hessian <- function(beta, X, Y){
  P <- 1 / (1 + exp(-X %*% beta))
  return(t(X) %*% diag(as.vector(P * (1 - P))) %*% X)
}


## Check convergence
converged <- function(curr, prev, df){
  tol <- qchisq(0.95, df, lower.tail = F) / 2
  abs_diff <- abs(curr - prev)
  within_atol <- abs_diff < tol
  within_rtol <- (abs_diff < tol * max(abs(curr), abs(prev)))
  return(within_atol && within_rtol)
}

## Newton's method main function
newton <- function(X, Y, maxIt = 100){
  p <- ncol(X)
  coefs <- rep(0, p)
  loglik_curr <- logit_loglik(coefs, X, Y)
  for (i in 1:maxIt) {
    loglik_prev <- loglik_curr

    gr <- as.matrix(logit_gradient(coefs, X, Y))
    h <- hessian(coefs, X, Y)
    coefs <- as.vector(coefs - solve(h) %*% gr)

    loglik_curr <- logit_loglik(coefs, X, Y)

    if (converged(loglik_curr, loglik_prev, df = p)){
      return(coefs)
    }
  }
  return("Newton's method not converging")
}
