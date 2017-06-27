#' Resampling Error \code{Resampling_Error}
#' 
#' @description \code{Resampling_Error} is designed to easily compute resampling error on design points
#' 
#' @details Compute resampling error of a surrogate model
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{Resampling_Error$new()}}{Creates a new \code{Resampling_Error}}
#'   \item{\code{get_val(surrogate_model)}}{ It evaluates the resampling error for a surrogate_model  (object \code{UPSM}).}
#' }
#' 
#'
#' @examples
#' library(UP)
#' d              <- 2
#' X      	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' y        	    <- apply(X, 1, branin)
#' upsm     	    <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,y,Scale =TRUE))
#' CV             <- Resampling_Error$new()          
#' PRESS_CV       <- CV$get_val(upsm)
#' print(" CV Resampling Error")
#' print(PRESS_CV)
#' 
#'      
#' @export
#' @format An \code{\link{R6Class}} generator object

Resampling_Error <- R6Class("Resampling_Error", 
               public 	= list(
                 initialize = function() 
                 {
                 },
                 get_val = function(surrogate_model){                   
                  error <- self$get_err(surrogate_model)
                   return((mean(error^2)))
                 }, 
                 get_err = function(surrogate_model){
                   scdata             <- surrogate_model$UP$Xscaled
                   numPoints   			  <- nrow(scdata)
                   pred               <- c()
                   if(!surrogate_model$ResIsSupproted) 
                   {
                     psubmodels 			<- surrogate_model$submodels
                     nsubmodel 			  <- length(psubmodels)
                     predictlist 		  <- function(listitem, newdata) {
                       listitem$predict(scdata)$mean
                     }
                     sub_predictions  <- matrix(0,numPoints,nsubmodel) 
                     if(nrow(scdata)>1600)
                     {
                       NumCore 	      <- detectCores() - 1
                       cl 			      <- makeCluster(NumCore) 
                       registerDoSNOW(cl)
                       foreach(i = 1:nsubmodel) %dopar% {
                         sub_predictions[,i]	<-  predictlist(psubmodels[[i]],scdata)
                       }
                       stopCluster(cl) 
                       rm(cl) 
                     }
                     else
                     {
                       for(i in 1:nsubmodel)
                       {
                         sub_predictions[,i]	<-  predictlist(psubmodels[[i]],scdata)
                       }
                     }
                     sub_predictions  	<- surrogate_model$UP$unscaleoutput(sub_predictions)
                     if(surrogate_model$UP$Resampling$resampling_type == "LOO" ) 
                     {
                       for(i in 1:nsubmodel){
                         pred[i] <- sub_predictions[i,i]
                       }
                     }
                     else
                     {
                       for(i in 1:nsubmodel){
                         pred[surrogate_model$UP$ListIndices[[i]]] <- sub_predictions[surrogate_model$UP$ListIndices[[i]],i]
                       }
                     }
                   }
                   else
                   {
                     master_prediction  <- surrogate_model$sm$predict(scdata)
                     sub_predictions 	  <- surrogate_model$sm$submodelspredictions(scdata, master_prediction = master_prediction)
                     sub_predictions  	<- surrogate_model$UP$unscaleoutput(sub_predictions)
                     for(i in 1:(nrow(sub_predictions))){
                       pred[i]          <- sub_predictions[i,i]
                     }
                   } 

                   error  <- pred - surrogate_model$UP$y
                   return(error)
                 }
               )
)
