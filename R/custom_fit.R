#' Create a fitness class with \code{custom_fit}
#' 
#' @description \code{custom_fit} is designed to estimate the quality of a surrogate
#' 
#' @details custom fit enable linear combination of several criteria 
#' @docType class
#' @importFrom R6 R6Class
#' @field listComp  List of component
#' @field dcoeff  Coefficient of each term fitness
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{custom_fit$new(coeff, components)}}{Creates a new \code{custom_fit}. Used to construct an assesment of a quality of a surrogate model}
#'   \item{\code{get_val(surrogate_model)}}{It gives an evaluation of a surrogate model (object \code{sm}).}
#' }
#' 
#'
#' @examples
#' library(UP)
#' d              <- 2
#' X      	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' y        	    <- apply(X, 1, branin)
#' upsm     	    <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,y,Scale =TRUE))
#' listCrit       <- list()
#' listCrit[[1]]  <- MSE$new() 
#' listCrit[[2]]  <- Resampling_Error$new() 
#' listCrit[[3]]  <- penlrm$new(X, y)
#' PPS            <- custom_fit$new(c(4,2,1), listCrit)
#' valPPS         <- PPS$get_val(upsm)
#' print("PPS")
#' print(valPPS)
#' 
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom lhs randomLHS

custom_fit <- R6Class("custom_fit", 
                    public = list(
                    dcoeff      = c(),
                    listComp    = NULL,
                    initialize  = function(dcoeff, listComp) 
                    {
                      self$dcoeff   <- dcoeff 
                      self$listComp <- listComp 
                    }, 
                    get_val = function(surrogate_model)
                    {
                      if(class(surrogate_model)[1] == "aggregation") 
                      {
                        return(surrogate_model$get_fitval())
                      }
                      num     <- length(self$dcoeff)
                      dval    <- 0
                      for(i in 1:num) 
                      {
                        dval  <- dval + self$dcoeff[i]*(self$listComp[[i]]$get_val(surrogate_model))
                      }                      
                      return(dval)
                    },
                    evaluations = function(surrogate_model){
                      list_res  <- list()
                      num       <- length(self$dcoeff)
                      for(i in 1:num) 
                      {
                        list_res[[i]]  <- self$listComp[[i]]$get_err(surrogate_model)
                      }                      
                      return(list_res)
                    },
                    get_ceoff = function(){
                      return(self$dcoeff)
                    },
                    getNumfitcrit = function(){
                      return(length(self$dcoeff))
                    }
                  )
)
