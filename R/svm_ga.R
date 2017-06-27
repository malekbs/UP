svm_ga <- R6Class("svm_ga", 
                  inherit = svmsm,
                  public = list( 
                  ## constructor 
                  initialize = function(x = NULL,y = NULL){
                      parameters <- list() 
                      randomnumbers<- runif(3) 
                      ## kernel 
                      ii        <- floor(randomnumbers[1]*length(private$kernels)) + 1 
                      parameters$kernel  <- private$kernels[ii]

                      ## costs 
                      ii        <- floor(randomnumbers[2]*length(private$costs)) + 1 
                      parameters$cost <- private$costs[ii]

                       ## epsilons 
                      ii        <- floor(randomnumbers[3]*length(private$epsilons)) + 1 
                      parameters$epsilon <- private$epsilons[ii]
 
                      self$parameters = parameters
                      super$initialize(x,y, parameters)
                  },
                  mutate = function()
                  {
                    parameters <- self$parameters 
                    mutatedgene <- floor(runif(1,1,4))  
                    if(mutatedgene == 1) 
                    {
                      randi  <- runif(1,0,1)
                      ii     <- floor(randi*length(private$kernels)) + 1 
                      parameters$kernel  <- private$kernels[ii]
                    }
                    if(mutatedgene == 2) 
                    {
                      randi  <- runif(1,0,1)
                      ii     <- floor(randi*length(private$costs)) + 1 
                      parameters$cost <- private$costs[ii]
                    } 
                    if(mutatedgene == 3) 
                    {
                      randi  <- runif(1,0,1)
                      ii     <- floor(randi*length(private$epsilons)) + 1 
                      parameters$epsilon <- private$epsilons[ii]
                    }
                       
                    self$parameters = parameters
                    super$initialize(x,y, parameters)
                 }, 
                crossover = function(parent_2)
                {
                    parameters   <- self$parameters 
                    parameters2  <- parent_2$getparameters()
                    mutatedgene  <- floor(runif(1,1,4))  
  
                    if(mutatedgene == 1) 
                    {
                       parameters$kernel  <- parameters2$kernel
                    }
                    if(mutatedgene == 2) 
                    { 
                       parameters$cost    <- parameters2$cost
                    }
                    if(mutatedgene == 3) 
                    {
                      parameters$epsilon  <- parameters2$epsilon
                    }
  
                     self$parameters = parameters
                     super$initialize(x,y, parameters)
                 },
                 show = function(){
                    print(paste("kernel", self$parameters$kernel)) 
                    print(paste("cost", self$parameters$cost)) 
                    print(paste("epsilon", self$parameters$epsilon)) 
                 }
              ), 
              private = list(
                    kernels = c("radial","sigmoid"),
                    costs = c(1.65,3,5,7.5,10),
                    epsilons = c(0.1,0.01,0.05)
              )
)
