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
