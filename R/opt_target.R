#' @import rgenoud
opt_target <-function(model,target =target, lower, upper, 
                      parinit=NULL, control=NULL, RefControl = NULL)
{
  d 					<- model$get_dimension()
  
  if (is.null(control$print.level)) 	control$print.level <- 0
  if(d<=4) N <- 3*2^d else N <- 12*d 
  if (is.null(control$BFGSmaxit)) 	control$BFGSmaxit <- N
  if (is.null(control$pop.size)) 		control$pop.size <- N
  if (is.null(control$solution.tolerance)) 	control$solution.tolerance <- 1e-8
  if (is.null(control$max.generations)) 		control$max.generations <- 10
  if (is.null(control$wait.generations)) 		control$wait.generations <- 2
  if (is.null(control$BFGSburnin)) 			control$BFGSburnin <- 2
  
  parinit 		<- NULL
  domaine 		<- cbind(lower, upper)
  iRefCrit 		<- 0
  if (is.null(RefControl$alpha)) 
    {
      if( class(model)[1]=="krigingsm")  
      {
        alpha   <- 0.0
      }
      else
      {
        alpha   <- 0.1
      }
    }
    else
    {
      alpha     <- RefControl$alpha 
    }
  
  o <- genoud(inverse_crit, nvars=d, max=TRUE, pop.size=control$pop.size, 
              max.generations=control$max.generations, wait.generations=control$wait.generations,
              hard.generation.limit=TRUE, starting.values=parinit, MemoryMatrix=TRUE, 
              Domains=domaine, default.domains=10, solution.tolerance=control$solution.tolerance, 
              boundary.enforcement=2, lexical=FALSE, gradient.check=FALSE, BFGS=TRUE,
              data.type.int=FALSE, hessian=FALSE, unif.seed=floor(runif(1,max=10000)),
              int.seed=floor(runif(1,max=10000)), print.level=control$print.level, share.type=0, 
              instance.number=0, output.path="stdout", output.append=FALSE, project.path=NULL, P1=50,
              P2=50, P3=50, P4=50, P5=50, P6=50, P7=50, P8=50,P9=0, P9mix=NULL,BFGSburnin=control$BFGSburnin,
              BFGSfn=NULL, BFGShelp=NULL,control=list("maxit"=control$BFGSmaxit),
              cluster=FALSE, balance=FALSE, debug=FALSE, model=model, target = target,alpha=alpha)

  o$par 					<- t(as.matrix(o$par))
  colnames(o$par) 		<- colnames(as.matrix(model$get_DOE()$x))

  o$value 				<- as.matrix(o$value)
  colnames(o$value)		<- "inverse_crit"

  return(list(par=o$par, value=o$value))
}
