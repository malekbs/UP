#' Adapter for UP surface
#'
#' A surrogate model ready to be used in a UP framework
#'
#' @param x  design input
#' @param y  design output
#' @param rs the surrogate model 
#' @param upcontrol list of resampling technique default NULL  
#'
#' @return A surrogate model ready to be used in a UP framework
#'
#' @examples
#' library(UP)
#' d            <- 2
#' X            <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' testdata     <- expand.grid(x1=s <- seq(0,1, length=10), x2=s)
#' Y            <- apply(X, 1, branin)
#' upsm         <- upsurface(x=X, y=Y, rs= krigingsm$new())
#' predictions  <- upsm$uppredict(testdata)
#'
#' @export
upsurface <- function(x,y, rs, upcontrol=NULL) 
{
	Scale 					  <- TRUE
	resampling_type   <- "LOO"
	ListIndices 		  <- NULL
	kfold 					  <- 10
	division_method   <- "random"

	if(!is.null(upcontrol$Scale)) 
		Scale 				  <- upcontrol$Scale
	if(!is.null(upcontrol$resampling_type)) 
		resampling_type <- upcontrol$resampling_type
	if(!is.null(upcontrol$ListIndices)) 
		ListIndices 	  <- upcontrol$ListIndices
	if(!is.null(upcontrol$kfold)) 
		kfold 				  <- upcontrol$kfold
	if(!is.null(upcontrol$division_method)) 
		division_method <- upcontrol$division_method

	UP     					  <- UPClass$new(x,y, Scale =Scale, resampling_type = resampling_type,
  								      ListIndices = ListIndices, kfold= kfold, division_method = division_method)
	UPSurrogatemodel  <- UPSM$new(rs,UP)

	return(UPSurrogatemodel)
}
 
 
