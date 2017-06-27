#' Refinement criteria
#'
#' Compute the refienemnt criteria for a set of points of the design space 
#'
#' @param x    points of the design space in which the criterion will be computed
#' @param model the surrogate model 
#' @param alpha  the value of the distance criteria default alpha = 0.001
#'
#' @return the value of the criterion
#'
#' @examples
#' #' library(UP)
#' d            <- 2;
#' n            <- 16
#' X    	      <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' Y            <- apply(X, 1, branin)
#' sm= krigingsm$new()
#' sm$setDOE(X,Y)
#' sm$train() 
#' ref_crit <- ref_crit(x= t(Xtest), model=sm,alpha=0) 
#' print(max(ref_crit))
#'
#' @export

ref_crit <- function(x, model, alpha = 0.001) {
  #######################################################################
  # Convert x in proper format(s)
  #######################################################################
  #	if (d != model$get_dimension()){ stop("x does not have the right size") }
  #	newdata.num 	<- as.numeric(x)
  newdata 		  <- data.frame(t(x))
 
  #colnames(newdata) = colnames(model$get_DOE()$x)
	if( class(model)[1]=="UPSM") 
	{
		###################################################################
		pred_x		<- model$uppredict(newdata) #predict.GPGA(object=model, newdata=newdata)$res
		res 		<- pred_x$upsd + alpha * pred_x$mindist
	}
	if( class(model)[1]=="krigingsm") 
	{
		res 					  <- 0
		if (alpha > 0.000001)  
		{
			X					    <- as.matrix(model$get_DOE()$x)
			vdMaxima			<- apply(X,2,max)
			vdMinima			<- apply(X,2,min)
			vdScaleSlope	<- 1/(vdMaxima-vdMinima)
			# vdScaleIntercept	<- -model@vdMinima*model@vdScaleSlope 
			## scaled DOE
			Xscaled				<- t(t(X)*vdScaleSlope )
			xdata         <- t(t(as.matrix(newdata))*vdScaleSlope)

			distMtx 			<- pdist2(xdata, Xscaled)
			MinDist 			<- apply(distMtx,1,min) 
			res 				  <- alpha*MinDist

			if (alpha ==1) { 
			  return(res)
			}
		}
		predx 					<- model$predict(newdata) 
		res 					  <- predx$sd + res
	}
	return(res)
}

