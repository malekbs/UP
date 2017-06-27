#'  Universal Prediction distribution based  Efficient Global Optimization UP-EGO 
#'
#'  Global optimization using the  Universal Prediction distribution 
#'
#' @param model the surrogate model 
#' @param fun  the real function
#' @param nsteps the number of points to be generated
#' @param lower the lower bound of the design space
#' @param upper the upper bound of the design space
#' @param seed the random seed (default = 1)
#' @param parinit inital points to be used in the optimization (default NULL)
#' @param control the optimization control parameters (default NULL)
#' @param EEIcontrol the optimization criterion parameters (default NULL)
#'
#' @return list of generated points and their values and the last updated surrogate model
#'
#' @examples
#' #' library(UP)
#' d            <- 2;
#' n            <- 16
#' X            <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Y            <- apply(X, 1, branin)
#' 
#' upsm     	<- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE) ) 
#' upego_res <-  upego( upsm,fun = branin, nsteps = 1, lower= c(0,0),upper = c(1,1) )
#' 
#' print( min(upego_res$last$get_DOE()$y ) ) 
#'
#' @export
upego <- function(model, fun, nsteps, lower, upper,  seed = 1,parinit=NULL, control=NULL,EEIcontrol = NULL) {
	 
	set.seed(seed) 
	n 		<- model$get_numpoints()
	initialminValue 	<- min(model$get_DOE()$y )
	currentmin 			<- initialminValue
	Xgenerated   		<- NULL
	ygenerated 			<- c()
	currentModel 		<- model
  for (i in 1:nsteps) {
    print(paste("OPTIMIZATION step :" ,i ,"..."))
    optimization_proc		<- opt_upei(model=currentModel, lower=lower, upper=upper, 
							parinit=parinit, control=control, EEIControl = EEIcontrol)

    xnew 					<- optimization_proc$par # (oRefProc$par - currentModel@vdScaleIntercept ) / currentModel@vdScaleSlope
    print("new point ")
    print(xnew)
	ynew      		<- fun(t(xnew))
	print(paste( "New value ", ynew))
	
	doe 			<- model$get_DOE()
	X			    <- rbind(doe$x, xnew)

	y			    <- c(doe$y, ynew )

    currentmin 		<- min(currentmin,ynew)
	Xgenerated			<- rbind(Xgenerated, xnew)
	ygenerated			<- c(ygenerated,  ynew)
    if(ynew == currentmin) 
    {
      print("new best at ")
      print(xnew)
    }
    print(paste( "current best value : ", currentmin, "  inital : ", initialminValue))
     
	model$setDOE(X,y)    
    model$train()
  }
  
  return(list(par=Xgenerated, 
              value= ygenerated,  nsteps=nsteps, lastmodel=currentModel))
}
