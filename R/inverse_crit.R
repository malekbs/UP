#' TMSE criteria
#'
#' Compute the target mean squared error for a set of points of the design space 
#'
#' @param x     points of the design space in which the criterion will be computed
#' @param model the surrogate model 
#' @param target  the target value
#' @param alpha  the value of the distance criteria default alpha = 0.001
#'
#' @return the value of the criterion
#'
#' @examples
#' library(UP)
#' d         <- 2
#' n         <- 16
#' X         <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest	   <- expand.grid(x1=seq(0,1,length=6), x2=seq(0,1,length=6))
#' Y         <- apply(X, 1, branin)
#' upsm      <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE))
#' crit      <- inverse_crit(x= t(Xtest), model=upsm)
#' print(max(crit))
#'
#' @export
inverse_crit <- function(x, model, target = 0.5, alpha = 0.1){
  newdata 	 <- data.frame(t(x))
  res        <- c()
  if( class(model)[1]=="UPSM") 
  {
    pred_x	 <- model$uppredict(newdata) 
    expdm    <- exp(-((pred_x$unsc_subpred- target)/0.1)^2/2)/sqrt(2*pi) 
    res 		 <- pred_x$coeff %*%t(expdm) + alpha * pred_x$mindist
  }

  return(res)
}


