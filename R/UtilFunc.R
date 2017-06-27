#' Post Processing and plot of the prediction and the UP variance 
#' of a 1 dimension functions.
#' 
#' @param xverif verification points in 1 dimension.
#' @param uppred UP prediction of xverif.
#' @param x design points inputs.
#' @param y design points outputs.
#' @return Plot the predictions.
#' @examples
#' library(UP)
#' x           <- as.matrix(c(-2.6,-0.2, 1.7,-1.4,1.2,3))
#' y           <- c(0.8, 0.5, 0.1, 0.3, 0, 0.4)
#' xverif      <- seq(-3, 3, length.out =300)
#' krig        <- krigingsm$new()
#' resampling  <- UPClass$new(x, y, Scale =TRUE) 
#' upsm        <- UPSM$new(sm= krig, UP= resampling) 
#' prediction  <- upsm$uppredict(xverif)
#' plotUP1D(xverif, prediction, x, y)
#' 
#' @export
plotUP1D <- function(xverif, uppred, x=NULL, y=NULL)
{
	lightblue  <- rgb(114/255,159/255,207/255,.3)
	darkbluetr <- rgb(32/255,74/255,135/255,.3)
	upsd       <- uppred$upsd
	pred       <- uppred$master_prediction
	minval     <- min(pred - 3*upsd)
  maxval     <- max(pred + 3*upsd)
	plot(xverif, uppred$master_prediction, "l", 
		col=darkbluetr, lwd = 2, ylim=c(minval,maxval),xlab="x",ylab="y")

	polygon(c(xverif,rev(xverif)), c(pred- 3*upsd,
		rev(pred + 3*upsd)), col=lightblue, border = NA)
	if(!is.null(x) && !is.null(x) )  
	{
		lines(x, y, "p", col ="black", pch = 15)
	}
}

#' contour plot of a function
#' 
#' @param fun a 2D function
#' @param partitions number of partirions per dimension
#' @param nlevel  number of line levels
#' @examples
#' plotContour2D(branin,40,40)
#' 
#' @export
plotContour2D <- function(fun, partitions, nlevel=40)
{
  NumSteps    <- partitions
  x           <- seq(0, 1, length.out = NumSteps)
  y           <- seq(0, 1, length.out = NumSteps)
  output      <- matrix(0,nrow=NumSteps, ncol=NumSteps)

  for (i in 1:NumSteps)
  {
    for (j in 1:NumSteps)
    { 
      output[i,j] <-  fun(c(x[i],y[j])) #,test_fun)
    }
  } 

  contour(x=x,y=y, z= output,  nlevels = nlevel)
}

