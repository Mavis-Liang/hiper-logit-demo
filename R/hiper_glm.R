#' @export hiper_glm
hiper_glm <- function(design, outcome, model='linear', option = list()){

  ## Check validity
  supported_models <- c('linear')
  if(!(model %in%  supported_models)){stop(sprintf("The model %s is not supported.",
                                                   model))
  }


  if (is.null(option$mle_solver)) {
    if (model == 'linear') {
      beta_est <- pseudoinverse_finder(design, outcome)
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
  return(hglm_out)
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


