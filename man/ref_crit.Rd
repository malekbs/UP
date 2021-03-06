% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_crit.R
\name{ref_crit}
\alias{ref_crit}
\title{Refinement criteria}
\usage{
ref_crit(x, model, alpha = 0.001)
}
\arguments{
\item{x}{points of the design space in which the criterion will be computed}

\item{model}{the surrogate model}

\item{alpha}{the value of the distance criteria default alpha = 0.001}
}
\value{
the value of the criterion
}
\description{
Compute the refienemnt criteria for a set of points of the design space
}
\examples{
#' library(UP)
d            <- 2;
n            <- 16
X    	      <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
Xtest	      <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
Y            <- apply(X, 1, branin)
sm= krigingsm$new()
sm$setDOE(X,Y)
sm$train() 
ref_crit <- ref_crit(x= t(Xtest), model=sm,alpha=0) 
print(max(ref_crit))

}

