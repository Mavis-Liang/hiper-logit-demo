#' @export hiper_glm
hiper_glm <- function(design, outcome, model='linear', option = list()){

  ## Check validity
  supported_models <- c("linear", "logit")
  if(!(model %in%  supported_models)){
    stop(sprintf("The model %s is not supported.", model))
  }


  if (is.null(option$mle_solver)) {
    if (model == 'linear') {
      beta_est <- pseudoinverse_finder(design, outcome)
    } else {
      beta_est <- newton(design, outcome)
    }
  } else {
    if(model == 'linear') {
      beta_est <- BFGS_finder_linear(design, outcome, option$mle_solver)
    } else{
      beta_est <- BFGS_finder_logit(design, outcome, option$mle_solver)
    }
  }

  ## Output
  hglm_out <- list(coef = beta_est)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}


coef(hiper_glm(design, outcome, model = "logit", option = list(mle_solver="BFGS")))
# 0.3850242  9.0003779 -7.8864191 -0.7692372

