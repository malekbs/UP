krig_ga <- R6Class("krig_ga", 
                     inherit = krigingsm,
                     public = list(
                       ## constructor 
                       initialize = function(x = NULL,y = NULL ){
                         parameters <- list()
                         randomnumbers<- runif(4) 
                         ## type 
                         ii        <- floor(randomnumbers[1]*length(private$types)) +1 
                         self$type <- private$types[ii]

                         ## kernel 
                         ii        <- floor(randomnumbers[2]*length(private$kernels)) +1 
                         parameters$covtype <- private$kernels[ii]

                         ## estim.method 
                         ii        <- floor(randomnumbers[3]*length(private$estimation_methods)) +1 
                         parameters$estim.method <- private$estimation_methods[ii]

                         ## formula 
                         ii        <- floor(randomnumbers[4]*length(private$trend)) +1 
                         oFormula <- ~.^2 
                         if(ii ==4) 
                         {
                           iDim      <- ncol(x) 
                           if (!is.null(iDim))  
                             oFormula  <- as.formula(paste(" ~ (", paste0("X", 1:iDim , collapse="+"), ")^2", "+", 
                                                         paste( "I(", paste0("X", 1:iDim), "^2)",  collapse="+"),  
                                                         collapse=""))
                         }
                         if(ii == 2) 
                         {
                           oFormula <- ~.
                         }
                         if(ii == 1) 
                         {
                           ii <- ~1
                         }
                         parameters$formula  <- oFormula

                         self$parameters = parameters
                         super$initialize(x,y, parameters, type=self$type)
                       },
                       mutate = function(){
                         parameters <- self$parameters 
                         mutatedgene <- floor(runif(1,1,5))  
                         if(mutatedgene == 1) 
                         {
                           randi  <- runif(1,0,1)
                           ii <- floor(randi*length(private$types)) +1 
                           self$type <- private$types[ii]
                         }
                         if(mutatedgene == 2) 
                         {
                           randi  <- runif(1,0,1)
                           ii <- floor(randi*length(private$kernels)) +1 
                           parameters$covtype <- private$kernels[ii]
                         } 
                         if(mutatedgene == 3) 
                         {
                           randi  <- runif(1,0,1)
                           ii <- floor(randi*length(private$estimation_methods)) +1 
                           parameters$estim.method <- private$estimation_methods[ii]
                         }
                         if(mutatedgene == 4) 
                         {
                           iDim     <- ncol(x)
                           ii       <- floor(randomnumbers[4]*length(private$trend)) +1 
                           oFormula <- as.formula(paste(" ~ (", paste0("X", 1:iDim , collapse="+"), ")^2", "+", 
                                                   paste( "I(", paste0("X", 1:iDim), "^2)",  collapse="+"),  
                                                   collapse=""))
                           if(ii == 3) 
                           {
                             oFormula <- ~.^2
                           }
                           if(ii == 2) 
                           {
                             oFormula <- ~.
                           }
                           if(ii == 1) 
                           {
                             oFormula <- ~1
                           }
                           parameters$formula  <- oFormula
                         }
                         self$parameters = parameters
                         super$initialize(x,y, parameters, type=self$type)
                       }, 
                       crossover = function(parent_2){
                         parameters   <- self$parameters 
                         parameters2  <- parent_2$getparameters()
                         mutatedgene  <- floor(runif(1,1,5))  
                         if(mutatedgene == 1) 
                         {
                           self$type                <- parameters2$type
                         }
                         if(mutatedgene == 2) 
                         { 
                           parameters$covtype       <- parameters2$covtype
                         }
                         if(mutatedgene == 3) 
                         {
                           parameters$estim.method  <- parameters2$estim.method
                         }
                         if(mutatedgene == 4) 
                         {
                           parameters$formula       <- parameters2$formula
                         }
                         self$parameters = parameters
                         super$initialize(x,y, parameters, type=self$type)
                       },
                       show  = function() {
                         print(paste("type", self$type)) 
                         print(paste("covtype", self$parameters$covtype)) 
                         print(paste("estim.method ", self$parameters$estim.method )) 
                         print(paste("formula", self$parameters$formula)) 
                       }
                     ), 
                     private = list(
				              types = c("UK","SK"),
                      trend = c("CONST","LINEAR","PURE_QUAD","CROSS_QUAD"),
                      kernels = c("gauss", "matern3_2", "matern5_2"),
				              estimation_methods = c("MLE","LOO")
                     )
)
