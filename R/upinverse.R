#' upinverse
#'
#' Generate  points to determine a line level  
#'
#' @param model the surrogate model 
#' @param fun  the real function
#' @param target a target value
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
#' library(lhs)
#' d            <- 2
#' n            <- 25
#' X            <- randomLHS(n,d)
#' Y            <- apply(X, 1, Tfunc)
#' upsm         <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE)) 
#' upinv_res    <-  upinverse(upsm, fun = Tfunc, target = 0.8,
#'                    nsteps = 15, lower= c(0,0),upper = c(1,1))
#'
#' @export

upinverse <- function(model, fun, target, nsteps, lower, upper, seed = 1,
                              parinit=NULL, control=NULL,RefControl = NULL)
{
  set.seed(seed) 
  n 				        <- model$get_numpoints()
  Xgenerated		    <- NULL
  ygenerated 		    <- c()
  target_scaled     <- model$UP$scaleoutput(target)
  for (i in 1:nsteps)
  {
    print(paste("UP-inverse iteration :" ,i))
    oRefProc    		<- opt_target(model, target = target_scaled, lower=lower, upper=upper,
                           parinit=parinit, control=control, RefControl=RefControl)
    xnew 				    <- oRefProc$par
    doe 				    <- model$get_DOE()
    X 					    <- rbind(doe$x, xnew)
    ynew 				    <- fun(t(xnew))
    y 					    <- c(doe$y, ynew)
    Xgenerated 			<- rbind(Xgenerated, xnew)
    ygenerated 			<- c(ygenerated, ynew)

    model$setDOE(X,y)   
    target_scaled   <- model$UP$scaleoutput(target)
    model$train()
  }

  return(list(par = Xgenerated, value = ygenerated,
              nsteps=nsteps, lastmodel=model))
}

