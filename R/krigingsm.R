#' Create a kriging   adapter class for the objects code{km} of package
#'  of the package \code{DiceKriging}.
#' 
#' @description \code{krigingsm} is a kriging adapter. It makes the use of  the universal distirbution easier 
#' 
#' @details It construct a kriging model from the package DiceKriging and it inherits form  the \code{sm}. 
#' The inheritance of the class  \code{sm} is required when one want to use the universal distribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import DiceKriging
#' @field model_name : Default "Kriging" 
#' @field type : universal kriging "UK" or simple kriging "SK"
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{krigingsm$new(x=NULL,y=NULL,parameters = NULL, type= "UK")}}{Creates a new \code{krigingsm} object. }
#'   \item{\code{train()}}{train the surrogate model.}
#'	 \item{\code{predict(newdata)}}{ it predicts the new data.}
#' }
#' 
#' @usage # kriging     <- krigingsm$new()
#'
#' @examples
#' library(UP)
#' d            <- 2;
#' n            <- 16
#' X    	      <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' Y            <- apply(X, 1, branin)
#' sm= krigingsm$new()
#' sm$setDOE(X,Y)
#' sm$train() 
#' sm$predict(Xtest)
#' 
#'
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
krigingsm <- R6Class("krigingsm", 
          		inherit = sm,
          		public 	= list(
          			## Kriging type SK, UK
          			type = "UK", 
          			## model_name invariant per type
          			model_name  			= "Kriging",
          			## constructor 
          			initialize = function(x = NULL,y = NULL,parameters = NULL, type= "UK"){
            				self$model  		  = NULL
            				self$parameters		= parameters
            
            				self$x				    = x
            				self$y 				    = y
            				self$fitness		  = 1e10
            				self$type 			  = type
          			},
          			train= function(){
          				if(!self$readytopredict) 
          				 {
            					super$train()
            					self$d= ncol(self$x)
            					self$n= nrow(self$x)
            
            					formula=~1 
            					if(!is.null(self$paramters$formula))  formula = self$paramters$formula
            
            					covtype 		  ="matern5_2"
            					if(!is.null(self$paramters$covtype))  covtype = self$paramters$covtype
            
            					coef.trend 		= NULL
            					if(!is.null(self$paramters$coef.trend))  coef.trend = self$paramters$coef.trend
            
            					coef.cov 		  = NULL 
            					if(!is.null(self$paramters$coef.cov))  coef.cov = self$paramters$coef.cov
            
            					coef.var 		  = NULL
            					if(!is.null(self$paramters$coef.var))  coef.var = self$paramters$coef.var
            
            					estim.method 	= "MLE"
            					if(!is.null(self$paramters$estim.method))  estim.method = self$paramters$estim.method
            
            					nugget 			  = NULL 
            					if(estim.method !="LOO")
            						nugget 		  = 1e-10
            
            					nugget.estim 	= FALSE
            					noise.var 		= NULL
            
            					penalty 		  = NULL
            					optim.method 	= "BFGS"
            
            					lower 			  = sqrt(self$d)*rep(0.05,self$d)
            					if(!is.null(self$paramters$lower)) lower= self$paramters$lower
            					upper 			  = sqrt(self$d)*rep(5, self$d)
            					if(!is.null(self$paramters$upper)) upper= self$paramters$upper
            
            					parinit 		  = NULL
            					multistart 		= 1
            					control 		  = list(trace=FALSE)
            					gr 				    = TRUE
            					iso 			    = FALSE
            					scaling			  = FALSE
            					knots			    = NULL
            					kernel			  = NULL	
            
            					self$model = km(formula=formula, design=self$x, response=self$y, covtype=covtype,
            										coef.trend =coef.trend, coef.cov=coef.cov, coef.var=coef.var,
            										nugget=nugget, nugget.estim=nugget.estim, noise.var=noise.var, 
            										estim.method=estim.method, penalty=penalty, optim.method=optim.method,
            										lower=lower, upper=upper, parinit=parinit, multistart=multistart,
            										control = control, gr = gr, iso = iso, scaling = scaling, knots= knots, 
            										kernel = kernel)
            
            					if(self$resampling_method == "LOO")
            					{
            						dmKinv						        <- tcrossprod(solve(self$model@T))
            						alphas 						        <- solve(crossprod(self$model@M)) 
            
            						private$dmBetaHatSysMtx 	<<- alphas %*% t(self$model@F) %*% dmKinv #  			 
            						private$MtxDiff 			    <<- dmKinv %*%(diag(self$n) - self$model@F%*%private$dmBetaHatSysMtx) #dmKinvBetaHatSys # 
            
            						private$eps 			        <<- leaveOneOut.km(self$model, type =self$type)$mean - as.vector(self$y)   
            						private$isEvalMtxComputed <<- TRUE
            						self$fast_computation 		<<- TRUE
            					}
          				}
          			},
          			predict=function(newdata){
          		 		colnames(newdata) = colnames(self$model@X)
          				return(predict.km(self$model,newdata, type=self$type))
          			},
          			submodelspredictions= function(newdata,master_prediction=NULL){
          				if (self$fast_computation) 
          				{
          					F.newdata	<- model.matrix(self$model@trend.formula, data = data.frame(newdata))  
          					dmalpha	 	<- F.newdata%*%private$dmBetaHatSysMtx + t(master_prediction$c)%*% private$MtxDiff 
          					values		<- t(t(dmalpha)*as.vector(private$eps))
          					return(master_prediction$mean + values)
          				}
          			},
          			supportFastComputation= function(Resampling){
          				if (Resampling$resampling_type == "LOO") 
          				{
          					self$fast_computation  <- TRUE
          					self$resampling_method <- "LOO"
          				}
          				return(self$fast_computation)
          			},
          			show = function(){
          				cat(paste0("surrogate type: ", self$model_name, ".\n"))
          				cat(paste0("parameter: ", self$parameters, ".\n"))
          			}
          		), 
          		private = list(
          				dmBetaHatSysMtx			= NULL, 
          				MtxDiff					    = NULL, 
          				isEvalMtxComputed		= NULL, 
          				eps						      = NULL 		
          		)
)
