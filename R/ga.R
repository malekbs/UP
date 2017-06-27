#' Genetic Algorithm to select best metamodel \code{ga}
#' 
#' @description \code{ga} is designed to estimate the quality of a surrogate
#' 
#' @details Automatic selection for general surrogates
#' @docType class
#' @importFrom R6 R6Class
#' @field listComp  List of component
#' @field dcoeff  Coefficient of each term fitness
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{ga$new(x, y)}}{Creates a new \code{custom_fit}. Used to construct an assesment of a quality of a surrogate model}
#'   \item{\code{evaluate(newdta)}}{It gives an evaluation of a surrogate model (object \code{sm}).}
#' }
#' 
#'
#' @examples
#' library(UP)
#' d              <- 2
#' X              <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' 
#' @export
#' @format An \code{\link{R6Class}} generator object

ga <- R6Class("ga", 
              public = list(
                fitnessfunc = NULL,
                x           = NULL,
                y           = NULL,
                listSM      = list(),
                randomseed  = 1,
                initialize  = function(x, y, fitnessfunc= NULL, types = NULL, randomseed=1, algo ="OS") 
                {
                  self$x    = x
                  self$y    = y
                  self$fitnessfunc = fitnessfunc
                  if(is.null(fitnessfunc)) 
                  {
                    listCrit          <- list()
                    listCrit[[1]]     <- MSE$new() 
                    listCrit[[2]]     <- Resampling_Error$new() 
                    listCrit[[3]]     <- penlrm$new(X, y)
                    PPS               <- custom_fit$new(c(4,2,1), listCrit)
                    self$fitnessfunc  <- PPS
                  }
                  if(!is.null(types))
                  {
                    private$listoftypes <- types 
                  }
                  self$randomseed = randomseed
                  private$generateDefault()
                },
                getbest = function(){
                  if(status != "solved") 
                     self$run() 
                  
                  return(self$listSM[[1]])
                }, 
                run = function() 
                {
                  
                },
                getsurrogate = function(i){
                  if(status != "solved") 
                    self$run() 

                  return(self$listSM[[i]])
                },
                get_fitnessfunc = function(){
                  return(self$fitnessfunc)
                }, 
                show  = function(){
                   print ("List Ensemble") 
                   for(i in 1:length(self$listSM)) 
                   {
                     self$listSM[[i]]$show()
                   }
                }
            ),
            private = list(
                status          = "Ready",
                types           = c("krig_ga", "svm_ga"),
                generateDefault = function(){
                   set.seed(self$randomseed) 
                   init_num_pertype = c(5,3) 
                   for(i in 1:init_num_pertype[1])
                   {
                     self$listSM <- c(self$listSM, krig_ga$new(self$x,self$y))
                   }
                   for(i in 1:init_num_pertype[2]) 
                   {
                     self$listSM <- c(self$listSM, svm_ga$new(self$x,self$y))
                   }
                },
                nextgeneration = function(){
                  ### Mutate
                  randi <- runif(1)
                  numMutatedGene <- floor(randi*length(self$listSM)) +1 
                  indices      <- floor(runif(numMutatedGene)*length(self$listSM)) +1 
                  for(i in 1:numMutatedGene){
                    aClone <- self$listSM[[indices[i]]]$clone()
                    aClone$mutate()
                    self$listSM <- c(self$listSM, aClone)
                  }

                  ### Cross Over
                  numCrossOver    <- 5
                  indices         <- floor(runif(2*numCrossOver)*length(self$listSM)) +1 
                  for(i in 1:numCrossOver){
                    parent1       <- self$listSM[[indices[i]]]$clone()
                    parent2       <- self$listSM[[indices[numCrossOver+i]]]$clone()
                   
                    if(class(parent1)[1] == class(parent2)[1]) 
                    {
                      parent1$crossover(parent2)
                      self$listSM <- c(self$listSM, parent1)
                    }
                    else
                    {
                      listsm      <- list()
                      listsm[[1]] <- parent1
                      listsm[[2]] <- parent2
                      parameters  <- list(listsm =listsm)
                      new_agg     <- aggregation$new(self$fitnessfunc, x, y, parameters = parameters)
                      self$listSM <- c(self$listSM, new_agg)
                    }
                  }
                  fitvect         <- lapply(self$listSM, self$fitnessfunc$get_val)
                  self$listSM     <- self$listSM[order(fitvect)]
                }
            )
)
