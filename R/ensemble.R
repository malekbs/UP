#' Create a kriging   adapter class for the objects code{km} of package
#' of the package \code{DiceKriging}.
#' 
#' @description \code{krigingsm} is a kriging adapter. It makes the use of  the universal distirbution easier 
#' 
#' @details It construct a kriging model from the package DiceKriging and it inherits form  the \code{sm}. 
#' The inheritance of the class  \code{sm} is required when one want to use the universal distribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @field numsm : number of sub models
#' @field model_name : "aggregation"
#' @field aggweights : aggregation weights
#' @field fit : fitness function used to tune the wieghts
#' @section Methods:
#'
#' \describe{
#'   \item{\code{krigingsm$new(x=NULL,y=NULL,parameters = NULL, type= "UK")}}{Creates a new \code{krigingsm} object. }
#'   \item{\code{train()}}{train the surrogate model.}
#'	 \item{\code{predict(newdata)}}{ it predicts the new data.}
#' }
#' 
#'
#' @examples
#' library(UP)
#' d            <- 2;
#' n            <- 16
#' X    	      <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' Xtest	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' Y            <- apply(X, 1, branin)
#' sm = krigingsm$new()
#' sm$setDOE(X,Y)
#' sm$train() 
#' sm$predict(Xtest)
#'
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
# @import DiceKriging

aggregation <- R6Class("aggregation", 
		  inherit = sm,
		  public 	= list(
			numsm 				    = 0, 
			## model_name invariant per type
			model_name  		  = "aggregation",
			aggweights   		  = NULL,
      fit               = NULL,
      fitness_value     = 1e10,
			## constructor 
			initialize = function(fit, x = NULL, y = NULL, parameters = NULL){
				self$parameters	= parameters
				self$numsm      = length(parameters$listsm)
				self$x				  = x
				self$y 				  = y
				self$fitness		= 1e10
				self$d 				  = ncol(self$x)
				self$n 				  = nrow(self$x)
				self$readytopredict = TRUE
        self$fit        = fit
			},
      #############################################################
			train = function(){
				if(!self$readytopredict)
				{
					super$train()
					for(i in 1:self$numsm){
						self$parameters$listsm[[i]]$train()
					}
				}
				self$computeweights()
			},
			#############################################################
			computeweights = function(){
			  num_fitcrit         <- self$fit$getNumfitcrit()
			  reslist             <- vector("list", num_fitcrit) 
			  for(i in 1:self$numsm) 
			  {
			    aRes              <- self$fit$evaluations(self$parameters$listsm[[i]])
			    for (j in 1:num_fitcrit){
			      reslist[[j]]    <- cbind(reslist[[j]], aRes[[j]])
			    }
			  }
        dcoeff              <- self$fit$get_ceoff() 
			  mtxx                <- matrix(0,self$numsm,self$numsm)
			  for(i in 1:self$numsm) 
			  {
			    mtxx              <- dcoeff[1]* (t(reslist[[1]])%*%reslist[[1]])/length(reslist[[1]])
			    for(j in 2:num_fitcrit){
            mtxx            <- mtxx + dcoeff[j]*(t(reslist[[j]])%*%reslist[[j]])/length(reslist[[i]])
			    }
			  }
				mtxinv				      <- solve(mtxx)
				self$aggweights 	  <- mtxinv%*%rep(1,self$numsm)/drop(rep(1,self$numsm)%*%mtxinv%*%rep(1,self$numsm)) 
        weights             <- as.vector(self$aggweights)
				self$fitness_value  <- drop(weights %*%mtxx%*%weights)
			},
			#############################################################
      mutate = function(){
        #parameters$listsm 
        self$train()
      },
			#############################################################
			predict = function(newdata){
			  newdata <- as.matrix(newdata)
				predictions 		  <- matrix(0,nrow(newdata), self$numsm)
				for(i in 1:self$numsm){
					predictions[,i]	<- self$parameters$listsm[[i]]$masterprediction(newdata)
				}
				meanpred  <-  predictions%*%self$aggweights 

		 		return(list(mean = meanpred, predictions = predictions))
			},
      get_fitval = function(){
        return(self$fitness_value)
      },
			get_fit = function(){
			  return(self$fit)
			},
			#############################################################
			uppredict = function(newdata){
			  list_resp         = self$parameters$listsm[[1]]$uppredict(newdata)  
			  master_prediction = list_resp$master_prediction *self$aggweights[1]
			  sub_predictions   = list_resp$sub_predictions*self$aggweights[1] 
			  mean              = list_resp$mean*self$aggweights[1] 
        unsc_subpred      = list_resp$unsc_subpred*self$aggweights[1] 
			  coeff             = list_resp$coeff
			  mindist           = list_resp$mindist 
			  for(i in 2:self$numsm){
          print(i)
			    list_resp	        = self$parameters$listsm[[i]]$uppredict(newdata)  
			    master_prediction = list_resp$master_prediction *self$aggweights[i] + master_prediction
			    sub_predictions   = list_resp$sub_predictions*self$aggweights[i] + sub_predictions
			    mean              = list_resp$mean*self$aggweights[i] + mean
			    unsc_subpred      = list_resp$unsc_subpred*self$aggweights[i] + unsc_subpred
			  } 
			  sumc    				<- apply(coeff,1,sum)
			  xbar    				<- apply(sub_predictions*coeff/sumc,1,sum)  
			  upsd    				<- sqrt(apply(coeff*(sub_predictions -xbar)^2/(sumc-1), 1, sum)) 
        
        return(list(master_prediction=master_prediction, 
			              sub_predictions =sub_predictions, 
			              coeff = coeff, upsd = upsd,mindist = mindist,
			              mean= mean, unsc_subpred = unsc_subpred))
			}, 
			#############################################################
			submodelspredictions = function(newdata,master_prediction=NULL){
				sub_predictions  	<- self$aggweights[1]*self$parameters$listsm[[1]]$submodelspredictions(newdata)
				for(i in 2:self$numsm){
					sub_predictions	<- sub_predictions + 
										self$aggweights[i]*self$parameters$listsm[[i]]$predict(newdata)$mean
				}
				return(sub_predictions)
			},
			#############################################################
			supportFastComputation = function(Resampling){
				return(self$fast_computation)
			},
			#############################################################
			show = function(){
				cat(paste0("surrogate type: ", self$model_name, ".\n"))
				cat(paste0("parameter: ", self$parameters, ".\n"))
			}
		)
)

