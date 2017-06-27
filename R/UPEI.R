#' Universal Prediction Expected Improvement UP-EI criteria
#'
#' Compute the universal preidction expected improvement 
#'
#' @param x     points of the design space in which the criterion will be computed
#' @param model the surrogate model 
#' @param plugin the current minimum value
#' @param alpha a parameter for exploration term 
#' @param up_method type of computation, default  "Empirical".  Other options: "UP_reg" regularized emprirical
#'                or "assume_gauss"  regularize UP as Gaussian ditribution.    
#' @param envir envirement variable 
#'
#' @return the value of the UP expected improvement
#'
#' @examples
#' #' library(UP)
#' d            <- 2
#' n            <- 16
#' X            <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest        <- expand.grid(x1=seq(0,1,length=6), x2=seq(0,1,length=6))
#' Y            <- apply(X, 1, branin)
#' upsm         <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE))
#' crit         <- UPEI(x= t(Xtest), model=upsm,plugin=min(Y)) 
#' print(max(crit))
#'
#' @export
UPEI <- function (x, model, plugin = NULL, alpha = 0.01, up_method = "Empirical", envir=NULL) {
  m <- min(model$get_DOE()$y)
  ########################################################################################
  # Convert x in proper format(s)
  #	if (d != model$get_dimension()){ stop("x does not have the right size") }
  #	newdata.num 	<- as.numeric(x)
  newdata 		  	<- data.frame(t(x))
  #colnames(newdata) = colnames(model$get_DOE()$x)
 
  ########################################################################################
  predx <- model$uppredict(newdata)
  res	<- 0
  rdiff <- m - predx$sub_predictions 
  diff 	<- apply(rdiff,2,max,0 )   
  if(up_method == "Empirical")
	{ 
		z 	<-  predx$coeff %*%diff 
		res <- drop(z)
		res <-  10*res + alpha *predx$mindist
	}
	if (up_method == "UP_reg") 
	{
		if (predx$mindist > 0.001)
		{ 
		  regulValues <-  (diff +1)*(pracma::erf(0.2*rdiff)+1)/2   
		  z   <- predx$coeff %*% t(as.matrix(regulValues))
		  res <- drop(z)     
		  res <- 10*res + alpha * predx$upsd  *  predx$mindist
		}
		else
		{
		  res<- 0
		}
	}
	if (up_method == "assume_gauss") 
	{    
		dstd  <- predx$upsd
		dmean <- apply(predx$sub_predictions ,2,mean )   
		if (dstd > 0.000001)
		{ 
		  cdfval <- 0.5*(1+pracma::erf((m- dmean)/(sqrt(2)*dstd)))
		  phival <-  exp(-((m-dmean)/dstd)^2)/sqrt(2*pi)
		  res <-  (m - dmean) *cdfval + dstd * phival
		}
		else
		{
		  res <-  max(m- dmean ,0)
		}
  }
  if (!is.null(envir)) 
  { 
    assign("predx", predx, envir=envir)  
    assign("diff", diff, envir=envir)  
  }
  return(res)
}
