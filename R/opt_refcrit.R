#' @import rgenoud
opt_refcrit <-function(model, lower, upper, parinit=NULL, control=NULL, RefControl = NULL) {
	d 	 <- model$get_dimension()

	if (is.null(control$print.level)) 	
    control$print.level <- 0
	if(d<=4) 
    N <- 3*2^d else N <- 12*d 
	if (is.null(control$BFGSmaxit)) 
    control$BFGSmaxit <- N
	if (is.null(control$pop.size)) 	
    control$pop.size <- N
	if (is.null(control$solution.tolerance)) 	
    control$solution.tolerance <- 1e-8
	if (is.null(control$max.generations)) 
    control$max.generations <- 10
	if (is.null(control$wait.generations)) 
    control$wait.generations <- 2
	if (is.null(control$BFGSburnin)) 
    control$BFGSburnin <- 2

	if (is.null(RefControl$alpha)) 
	{
		if( class(model)[1]=="krigingsm")  
		{
			alpha   <- 0.0
		}
		else
		{
			alpha   <- 0.001
		}
	}
	else
	{
		alpha     <- RefControl$alpha 
	}
	
	parinit 		<- NULL
	domaine 		<- cbind(lower, upper)
	iRefCrit 		<- 0

	o <- genoud(ref_crit, nvars=d, max=T, pop.size=control$pop.size, max.generations=control$max.generations,
              wait.generations=control$wait.generations, starting.values=parinit, Domains=domaine,
              default.domains=10, solution.tolerance=control$solution.tolerance, boundary.enforcement= 2, 
              gradient.check=F, unif.seed=floor(runif(1,max=10000)), int.seed= floor(runif(1,max=10000)), 
              print.level=control$print.level, BFGSburnin=control$BFGSburnin, 
              control=list("maxit"=control$BFGSmaxit), model=model, alpha = alpha)

	o$par 					  <- t(as.matrix(o$par)) 
	colnames(o$par) 	<- colnames( as.matrix(model$get_DOE()$x))

	o$value 				  <- as.matrix(o$value)
	colnames(o$value)	<- "ref_crit"

 
	return(list(par=o$par, value=o$value)) 
}
