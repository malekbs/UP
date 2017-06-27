#' analytical functions
#' 
#' \code{branin} - A test function 
#' @param X points coordinates
#' 
#' @return \code{branin} - a real value.
#' @rdname analytical_function
#' @export 
#' 
branin <- function(X){
  x1 <- X[1]*15-5   
  x2 <- X[2]*15     
  (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}


#' analytical functions
#' 
#' \code{Tfunc} - A test function. 
#' 
#' @return \code{Tfunc} - a real value.
#' @rdname analytical_function
#' @export


Tfunc <- function(X){
  y <- X[2]*5 - 4 
  x <- X[1]*4 - 2 
  t  <-   
  if(x>0.2){
    t <- x- 0.05
  }
  else{
    t <- min(max(-x-0.2,0), max(abs(y)-0.2 ,0))
  }

  return(exp(-3*t^2 ))
}

