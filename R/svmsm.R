#' Create a support vector machine (svm) adapter class for  the objects  code{svm} of  the package \code{e1071}.  
#' 
#' @description \code{svmsm} is a svm adapter. It makes the use of  the universal distirbution easier 
#' 
#' @details It construct a svm model adapter  for  the objects  code{svm} of  the package \code{e1071}  and it inherits the class suurogate model \code{sm}. 
#' The inheritance of the class \code{sm} is required when one want to use the universal distribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @field model_name :"svm" 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{svmsm$new(x=NULL,y=NULL,parameters = NULL )}}{Creates a new \code{svmsm} object. }
#'   \item{\code{train()}}{train the surrogate model.}
#'	 \item{\code{predict(newdata)}}{it predicts the new data.}
#' }
#' 
#' @usage # svm     <- svmsm$new()
#'
#' @examples
#' library(UP)
#' d      <- 2
#' X    	<- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest	<- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' Y      <- apply(X, 1, branin)
#' sm     <- svmsm$new()
#' sm$setDOE(X,Y)
#' sm$train() 
#' predictions <- sm$predict(Xtest)
#' 
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @import e1071

svmsm <- R6Class("svmsm", 
			inherit 	= sm,
			public 	= list(
			## model_name invariant per type
			model_name  			= "SVM",
			initialize = function(x = NULL, y = NULL, parameters = NULL) 
			{
				self$model  		= NULL
				self$parameters	= parameters

				self$x				  = x
				self$y 				  = y
				self$fitness		= 1e10
			},
			train = function() 
			{
				if(!self$readytopredict) 
				{
					super$train()
					formula     = ~1 
					if(!is.null(self$parameters$formula))
						formula = self$parameters$formula

					gamma 		= 0.5
					if(!is.null(self$parameters$gamma))
						gamma   = self$parameters$gamma

					epsilon 	= 0.1
					if(!is.null(self$parameters$epsilon))
						epsilon   = self$parameters$epsilon

					degree 		= 2
					if(!is.null(self$parameters$degree))
						degree  = self$parameters$degree
 
					tolerance 	= 0.0001
					if(!is.null(self$parameters$tolerance))
						tolerance = self$parameters$tolerance

					cost 		= 1.65
					if(!is.null(self$parameters$cost))
						cost    = self$parameters$cost

					shrinking 	= FALSE
					if(!is.null(self$parameters$shrinking))
						shrinking = self$parameters$shrinking
          
					self$model	= e1071::svm(self$x, self$y, gamma = gamma, 
        										epsilon = epsilon, degree = degree,
        										tolerance = tolerance, cost = cost, 
        										shrinking = shrinking)
				}
			},
			predict=function(newdata) 
			{
				return(list(mean= predict(self$model,newdata)))
			},
			submodelspredictions= function(){

			},
			supportFastComputation= function(Resampling){
			  return(self$fast_computation)
			},
			show = function(){
				cat(paste0("surrogate type: ", self$model_name, ".\n"))
				cat(paste0("parameter: ", self$parameters, ".\n"))
			}
		),
		private = list(
			settingstype 	= NULL,
			settingsChoices = NULL,
			generateDefault = function()
			{
				settingstype 	<- c("formula", "degree","cost")
				settingsChoices	<- cbind(c("radial","sigmoid","NULL","NULL"),
                                         c("1","2","3","NULL"), 
                                         c("1","1.65","2.5","5"))
			}
		)
)

