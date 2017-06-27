#' Mean Square Error \code{MSE}
#' 
#' @description \code{MSE} is designed to easily compute MSE on design points
#' 
#' @details Compute MSE of a surrogate model
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{MSE$new()}}{Creates a new \code{MSE}}
#'   \item{\code{get_val(surrogate_model)}}{ It evaluates the MSE for a surrogate_model  (object \code{sm}).}
#' }
#' 
#'
#' @examples
#' library(UP)
#' d            <- 2
#' X    		    <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' y        	  <- apply(X, 1, branin)
#' upsm     	  <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,y,Scale =TRUE))
#' mse          <- MSE$new()          
#' valmse       <- mse$get_val(upsm)
#' print("MSE")
#' print(valmse)
#' 
#'      
#' @export
#' @format An \code{\link{R6Class}} generator object

MSE <- R6Class("MSE", 
                  public 	= list(
                    initialize = function() 
                    {
                    },  
                    get_val = function(surrogate_model){
                      xy  <- surrogate_model$get_DOE()
                      x   <- xy$x
                      y   <- xy$y
                      error   <- surrogate_model$masterprediction(x) - y
                      return((mean(error^2)))
                    }, 
                    get_err = function(surrogate_model){
                      xy  <- surrogate_model$get_DOE()
                      x   <- xy$x
                      y   <- xy$y
                      error   <- surrogate_model$masterprediction(x) - y 
                      return(error)
                    }
                  )
)
