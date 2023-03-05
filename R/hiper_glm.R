
####################### Helper functions ######################

########## For cholesky ########

chol_finder <- function(design, outcome){
  XTX <- t(design) %*% design
  mle_coef <- solve(XTX, t(design) %*% outcome)
  return(as.vector(mle_coef))
}


############ FOR BFGS ############
## The log-likelihood function
gaussian_logLi <- function(beta, X, Y, noise_var = 1){

  f <- -0.5 * sum((Y - X %*% beta)^2) / noise_var
  return(f)
}
## The gradient
gradient <- function(beta, X = design, Y = outcome){

  gr = as.vector(t(X) %*% (Y - X %*% beta))
  return(gr)
}

##
BFGS_finder <- function(design, outcome, method){
  init_coef <- rep(0, ncol(design))

  obj_fn <- function(coef) {
    gaussian_logLi(coef, design, outcome)
  }

  obj_grad <- function(coef){
    gradient(coef, design, outcome)
  }

  BFGS <- stats::optim(init_coef,
                       obj_fn, obj_grad,
                       method = method,
                       control = list(fnscale = -1))
  return(BFGS$par)
}


##################### Exported functions ####################
#' @export hiper_glm
hiper_glm <- function(design, outcome, model='linear', option = list()){

  ## Check validity
  supported_models <- c('linear')
  if(!(model %in%  supported_models)){stop(sprintf("The model %s is not supported.",
                                                   model))
  }

  warning("The function is continuing to be implemented")

  if (is.null(option$mle_solver)) {
    if (model == 'linear') {
      beta_est <- chol_finder(design, outcome)
    } else {
      # TODO: implement iteratively reweighted least-sq
      stop("Not yet implemented.")
    }
  } else {
    beta_est <- BFGS_finder(design, outcome, option$mle_solver)
  }

  ## Output
  hglm_out <- list(coef = beta_est)
  class(hglm_out) <- "hglm"
  hglm_out
}


#' @export coef.hglm
coef.hglm <- function(hglm_out){
  return(hglm_out$coef)
}

#' @export vcov.hglm
vcov.hglm <- function(hglm_out){
  return(diag(length(hglm_out$coef)))
}

#' @export print.hglm
print.hglm <- function(hglm_out){
  return("printing...")
}


