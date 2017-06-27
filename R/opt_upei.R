#' @import rgenoud
opt_upei <-function(model, plugin=NULL,lower, upper, parinit=NULL , control=NULL, EEIControl = NULL)
{  
  EEI.env = NULL
  #EEI.env<- new.env()  
  #environment(EEI) <-   environment(EEI.grad) <-  EEI.env 
  #gr = EEI.grad
  doe <- model$get_DOE()
  plugin <- min(doe$y)
  minimization <- FALSE
 
  d <- ncol(doe$x)
  
  if (is.null(control$print.level)) control$print.level <- 0
  if(d<=5) N <- 3*2^d else N <- 32*d 
  if (is.null(control$BFGSmaxit)) control$BFGSmaxit <- N
  if (is.null(control$pop.size))  control$pop.size <- N
  if (is.null(control$solution.tolerance))  control$solution.tolerance <- 1e-5
  if (is.null(control$max.generations))  control$max.generations <- 12
  if (is.null(control$wait.generations))  control$wait.generations <- 2
  if (is.null(control$BFGSburnin)) control$BFGSburnin <- 2
  if (is.null(parinit))  parinit <- lower + runif(d)*(upper-lower)
  
  if (is.null(EEIControl$alpha)) alpha <- 0.001
  else
    alpha <- EEIControl$alpha
  if (is.null(EEIControl$up_method))  up_method <- "Empirical"
  else
    up_method<-EEIControl$up_method
   
  domaine <- cbind(lower, upper)
  
  o <- genoud(UPEI, nvars = d, max = TRUE,
              pop.size = control$pop.size, max.generations = control$max.generations, wait.generations = control$wait.generations,
              hard.generation.limit = TRUE, starting.values = parinit, MemoryMatrix = TRUE, 
              Domains=domaine, default.domains = 10, solution.tolerance = control$solution.tolerance, 
             # gr = gr,
              boundary.enforcement = 2, lexical = FALSE, gradient.check = FALSE, BFGS = TRUE,
              data.type.int = FALSE, hessian = FALSE, unif.seed = floor(runif(1,max=10000)), int.seed = floor(runif(1,max=10000)), 
              print.level = control$print.level,  
              share.type = 0, instance.number = 0, output.path = "stdout", output.append = FALSE, project.path = NULL,
              P1=50, P2=50, P3=50, P4=50, P5=50, P6=50, P7=50, P8=50, P9=0, P9mix = NULL, 
              BFGSburnin = control$BFGSburnin, BFGSfn = NULL, BFGShelp=NULL, control=list("maxit"=control$BFGSmaxit), 
              cluster=FALSE, balance=FALSE, debug=FALSE,
              model=model,plugin=plugin, alpha = alpha,up_method = up_method, envir=EEI.env)
 
  o$par <- t(as.matrix(o$par))
  colnames(o$par) 	<- colnames( as.matrix(model$get_DOE()$x))
  o$value <- as.matrix(o$value)
  colnames(o$value) <- "up_ei"  
  
  return(list(par=o$par, value=o$value)) 
}
