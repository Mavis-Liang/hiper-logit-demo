#' @export hiper_glm
hiper_glm <- function(design, outcome, model='linear', option = list()){

  ## Check validity
  supported_models <- c("linear", "logit")
  if(!(model %in%  supported_models)){
    stop(sprintf("The model %s is not supported.", model))
  }

  if (model == 'linear') {
    if (is.null(option$mle_solver)) {
      beta_est <- pseudoinverse_finder(design, outcome)
    } else{
      beta_est <- BFGS_finder_linear(design, outcome, option$mle_solver)
    }

  } else if (model == 'logit') {
    if (is.null(option$mle_solver)) {
      if (is.null(option$newton_solver)){
        beta_est <- newton(design, outcome)
      }else{
        beta_est <- newton(design, outcome, solver = option$newton_solver)
      }
    } else {
      beta_est <- BFGS_finder_logit(design, outcome, option$mle_solver)
    }
  }

  ## Output
  hglm_out <- list(coef = beta_est)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}



