#'  UPSM is a class for universal distribution of a surrogate model
#' 
#' @description \code{UPSM} is a class for a surrogate model 
#' ready to be used in a UP framework.
#' 
#' @details It contains a surrogate model and a UPClass. 
#' The UPSM sets the set of submodels and perform UP predictions.
#'
#' @docType class
#' @importFrom R6 R6Class 
#' @import doSNOW
#' @import foreach
#' @field sm: a surrogate model of class \code{sm}
#' @field UP: a class \code{UPClass}
#' @field submodels: list of submodels
#'
#' @section Methods: 
#' \describe{
#'   \item{\code{UPSM$new(sm = NULL, UP=NULL)}}{Creates a new \code{UPSM} object, 
#'                  and intialize the submodels. }
#'   \item{\code{uppredict(newData)}}{The UP prediction of the new data.}
#' }
#' 			
#' @examples
#' 
#' library(UP)
#' d          	<- 2
#' n           	<- 16
#' testdata    	<- expand.grid(x1=s <- seq(0,1, length=10), x2=s)
#' X    		    <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' y        	  <- apply(X, 1, branin)
#' upsm     	  <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,y,Scale =TRUE))
#' predictions  <- upsm$uppredict(testdata)
#'
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
UPSM <- R6Class("UPSM", 
  		public = list(
  		### Surrogate model
  		sm        = NULL, 
  		### Resampling UP method 
  		UP        = NULL, 
  		### List of submodels 
  		submodels = NULL, 
  		### Does the surrogate model support
  		### closed form forumla for the resampling technique? 
  		ResIsSupproted  = FALSE,
  		initialize = function(sm = NULL, UP=NULL){
  			  self$sm = sm 
  			  self$UP = UP
  			  self$train()
  		},
  		setDOE = function(x,y){
  				UP <- self$UP$clone(deep = TRUE)
  				UP$setDOE(x,y)
  				self$UP <- UP
  		},
  		masterprediction = function(newdata){
  		    scnewdata   			  <- self$UP$scaleinput(newdata) 
  		    master_prediction   <- self$sm$predict(scnewdata)
  		    masterpred          <- master_prediction$mean
  		    master_prediction		<- self$UP$unscaleoutput(masterpred)
  		    return(master_prediction)
  		},
  		uppredict = function(newdata){
  			  scnewdata 				  <- self$UP$scaleinput(newdata) 
  			  coeffanddist 			  <- self$UP$computeweights(scnewdata)
  			  numPoints 				  <- nrow(newdata)
  			  coeff 					    <- coeffanddist$coeff
  			  master_prediction   <- self$sm$predict(scnewdata)
    			if(!self$ResIsSupproted) 
    			{
    				psubmodels 			  <- self$submodels
    				nsubmodel 			  <- length(self$submodels)
    				predictlist 		  <- function(listitem, newdata){
    					listitem$predict(scnewdata)$mean
    				}
    				sub_predictions   <- matrix(0,numPoints,nsubmodel) 
    				if(nrow(scnewdata)>1600){
    					NumCore 	      <- detectCores() - 1
    					cl 			        <- makeCluster(NumCore) #
    					registerDoSNOW(cl)
    					foreach(i = 1:nsubmodel) %dopar% {
    						sub_predictions[,i]	<- predictlist(psubmodels[[i]],scnewdata)
    					}
    					stopCluster(cl) 
    					rm(cl) 
    				}
    				else
    				{
    					for(i in 1:nsubmodel)
    					{
    						sub_predictions[,i]	<- predictlist(psubmodels[[i]],scnewdata)
    					}
    				}
    				fin <- proc.time() 
    			}
    			else
    			{
    				sub_predictions 	<- self$sm$submodelspredictions(scnewdata, master_prediction = master_prediction)
    			} 
    			upsd					      <- c()
    	    unsc_subpred        <- sub_predictions
    			sub_predictions			<- self$UP$unscaleoutput(sub_predictions)
    		  masterpred          <- master_prediction$mean
    			master_prediction		<- self$UP$unscaleoutput((masterpred)) 
    			sumc    				    <- apply(coeff,1,sum)
    			xbar    				    <- apply(sub_predictions*coeff/sumc,1,sum)  
    			upsd    			      <- sqrt(apply(coeff*(sub_predictions -xbar)^2/(sumc-1), 1, sum)) 
  
  			return(list(master_prediction= master_prediction, 
  						  sub_predictions = sub_predictions, 
  						  coeff = coeff, upsd = upsd,mindist = coeffanddist$mindist,
                mean= masterpred, unsc_subpred = unsc_subpred))
  		},
  		train = function(){ 
    		## handle the LOO case (default)          
  			self$ResIsSupproted <- self$sm$supportFastComputation(self$UP$Resampling)
  			x                   <- self$UP$Xscaled 
  			y                   <- self$UP$Yscaled
  			if(!self$ResIsSupproted) 
  			{
  				if(self$UP$Resampling$resampling_type == "LOO") 
  				{
  					UP <- self$UP
  					sm <- self$sm
  					n  <- self$UP$n
  					submodels =  vector("list", n) 
  					for(i in 1:n){
  						submodels[[i]]    <- sm$clone(deep = TRUE)
  						submodels[[i]]$setDOE(UP$Xscaled[-i,], UP$Yscaled[-i])	 
  						submodels[[i]]$train()
  					}
  					self$submodels 	    <- submodels 
  				}
  				else 
  				{
  					UP 						      <- self$UP
  					sm 						      <- self$sm
  					n 						      <- length(self$UP$Resampling$ListIndices)
  					submodels 		      <- vector("list", n)
  					if(nrow(x)>100)
  					{
  						for(i in 1:n){
  							submodels[[i]] 	<- sm$clone(deep = TRUE)
  							submodels[[i]]$setDOE(x[-UP$Resampling$ListIndices[[i]],], y[-UP$Resampling$ListIndices[[i]]])	
  						}
  						NumCore 			    <- detectCores() - 1
  						cl 					      <- makeCluster(NumCore) # for 4 cores machine 
  						clusterEvalQ(cl, library(R6))
  						clusterEvalQ(cl, library(e1071))
  						clusterEvalQ(cl, library(DiceKriging))
  						clusterExport(cl, c('krigingsm','svmsm'))
  
  						submodels         <- clusterApply(cl, submodels, function(nms){
  							nms$train()
  							nms
  						})
  						stopCluster(cl)
  						rm(cl)
  					}
  					else 
  					{
  						for(i in 1:n){
  							submodels[[i]] 	<- sm$clone(deep = TRUE)
  							submodels[[i]]$setDOE(x[-UP$Resampling$ListIndices[[i]],], 
                                        y[-UP$Resampling$ListIndices[[i]]])	
  							submodels[[i]]$train()
  						}
  					}
  					self$submodels 			<- submodels 
  				}
  				self$sm$setDOE(x,y)
  				self$sm$train()
  			}
  			else
  			{	
  				self$sm$setDOE(x,y)
  				self$sm$train()
  				submodels <- NULL
  			}
  		},
  		get_numpoints = function(){
  			return (self$UP$get_numpoints())
  		},
  		get_dimension = function(){
  			return (self$UP$get_dimension())
  		},
  		get_DOE = function(){
  			return(self$UP$get_DOE())
  		}
	)
)
