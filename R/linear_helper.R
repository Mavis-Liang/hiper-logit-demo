## pseudo inverse MLE finder
pseudoinverse_finder <- function(design, outcome){
  XTX <- t(design) %*% design
  mle_coef <- qr.solve(XTX, t(design) %*% outcome)
  return(as.vector(mle_coef))
}

## BFGS
## The log-likelihood function
gaussian_loglik <- function(beta, X, Y, noise_var = 1){

  return(-0.5 * sum((Y - X %*% beta)^2) / noise_var)
}

## The gradient
linear_gradient <- function(beta, X, Y){
  gr = as.vector(t(X) %*% (Y - X %*% beta))
  return(gr)
}


## BFGS
BFGS_finder_linear <- function(design, outcome, method){
  init_coef <- rep(0, ncol(design))

  obj_fn <- function(coef) {
    gaussian_loglik(coef, design, outcome)
  }

  obj_grad <- function(coef){
    linear_gradient(coef, design, outcome)
  }

  BFGS <- stats::optim(init_coef,
                       obj_fn, obj_grad,
                       method = method,
                       control = list(fnscale = -1))
  return(BFGS$par)
}



