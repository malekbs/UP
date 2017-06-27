#' UP-SMART 
#'
#' Enhance the overall prediction capabilities of a surrogate model by the Universal Prediction 
#' distribution based Surrogate Modeling Adapative Refinement Strategy UP-SMART
#'
#' @param model the surrogate model 
#' @param fun  the real function
#' @param nsteps the number of points to be generated
#' @param lower the lower bound of the design space
#' @param upper the upper bound of the design space
#' @param seed the random seed (default = 1)
#' @param parinit inital points to be used in the optimization (default NULL)
#' @param control the optimization control parameters (default NULL)
#' @param RefControl the refienement criterion parameters (default NULL)
#'
#' @return list of generated points and their values and the last updated surrogate model
#'
#' @examples
#' #' library(UP)
#' d            <- 2;
#' n            <- 16
#' X            <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest        <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' Y            <- apply(X, 1, branin)
#' sm           <- krigingsm$new()
#' sm$setDOE(X,Y)
#' sm$train() 
#' upsmart_res  <- upsmart(sm,fun = branin,nsteps = 5, lower= c(0,0),upper = c(1,1))
#' 
#' print(upsmart_res$last$get_DOE())
#'
#' @export


upsmart <- function(model, fun, nsteps, lower, upper, seed = 1,
                          parinit=NULL, control=NULL,RefControl = NULL) 
{
	set.seed(seed) 
	n 				<- model$get_numpoints()
	Xgenerated		<- NULL
	ygenerated 		<- c()

	for (i in 1:nsteps)
	{
		print(paste("UP-SMART iteration :" ,i))
		oRefProc    		<- opt_refcrit(model, lower=lower, upper=upper, parinit=parinit,
											control=control, RefControl=RefControl)
		xnew 				<- oRefProc$par
		doe 				<- model$get_DOE()
		X 					<- rbind(doe$x, xnew)
		ynew 				<- fun(t(xnew))
		y 					<- c(doe$y, ynew)
		Xgenerated 			<- rbind(Xgenerated, xnew)
		ygenerated 			<- c(ygenerated, ynew)
		
		model$setDOE(X,y)    
		model$train()
	}

	return(list(par= Xgenerated, value= ygenerated,
                      nsteps=nsteps, lastmodel=model))
}
