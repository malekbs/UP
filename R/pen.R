#' Create a penalty class with \code{penlrm}
#' 
#' @description \code{penlrm} is designed to generate extra data and estimate its values piecewise linear. 
#' Its is often used as a penalty that penalizes high surrogate model oscillations.
#' 
#' @details Construct data points within some simplices of the design od ecperiments 
#'  and evaluate a \code{sm} object with regards to the descrepency between its prediction and the generated data
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @field Xpen the witness data generated to perform comparison
#' @field Ypen the piecewise linear estimation
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{penlrm$new(x,y)}}{Creates a new \code{penlrm}. Often used to construct the penalty measure and generate its data.}
#'   \item{\code{get_val(surrogate_model)}}{ It evaluates the penalty for a surrogate_model (object \code{sm}).}
#'   \item{\code{get_err(surrogate_model)}}{ It evaluates the errors of a surrogate model (object \code{sm}).}
#' }
#' 
#'
#' @examples
#' library(UP)
#' X   <- matrix(1:40,20,2)  
#' Y   <- runif(20)
#' pen <- penlrm$new(X, Y)
#' 
#' x      	<- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
#' y        <- apply(x, 1, branin)
#' upsm     <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(x,y,Scale =TRUE))
#' 
#' lrmval   <- pen$get_val(upsm)
#' print("val_lrm")
#' print(lrmval)
#' 
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom lhs randomLHS

penlrm <- R6Class("penlrm", 
		public 	= list(
			## 
			Xpen          = NULL,
			Ypen          = NULL,
			d             = 0,
			initialize= function(x ,y ) 
			{ 
				self$d      <- ncol(x)
				upper 			<- apply(x,2,max)
				lower 			<- apply(x,2,min)
			 	n				    <- nrow(x)
				numsimplices<- n-1 
				extradata 	<- randomLHS(numsimplices, self$d)
				extradata 	<- extradata * (upper-lower) + lower
        
				distmtx 		<- (pdist2(as.matrix(extradata), as.matrix(x)))
				ordermtx 		<- apply(distmtx,1,order) 
				coeff			  <- 1/self$d

				newpoints   <- as.matrix(x[ordermtx[1,],]) 
				ylrm			  <- y[ordermtx[1,]]
				for(p in 2:(self$d+1)) 
				{
					newpoints <- newpoints + as.matrix(x[ordermtx[p,],]) 	
					ylrm		  <- ylrm + y[ordermtx[p,]]
				}
				newpoints 	<- newpoints/(self$d+1)
				ylrm		    <- ylrm/(self$d+1)
				for(p in 1:(self$d+1)) 
				{
					np 		    <- 2* as.matrix(x[ordermtx[p,],]) 
					yp 		    <- 2* y[ordermtx[p,]]
					for (ii in 1:(self$d+1) ) 
					{ 
						if(ii != p) 
						{
							np   	<- np + coeff*as.matrix(x[ordermtx[ii,],]) 	
							yp		<- yp + coeff*y[ordermtx[ii,]]
						}
					}
					np 	 		  <- np/3
					yp			  <- yp/3
					newpoints <- rbind(newpoints, np)
					ylrm 		  <- c(ylrm, yp)
				}
				self$Xpen 	<- newpoints
				self$Ypen 	<- ylrm
			},
			get_val = function(surrogate_model){
				error       <- surrogate_model$masterprediction(self$Xpen) - self$Ypen
				return((mean(error^2)))
			}, 
			get_err = function(surrogate_model){
			  error       <- surrogate_model$masterprediction(self$Xpen) - self$Ypen
        return(error)
			}
		),
		private = list(
		generatedata = function( extradata,x,y,numsimplices ) 
		{
			distmtx 	    <- (pdist2(as.matrix(extradata), as.matrix(x)))
			ordermtx 	    <- apply(distmtx,1,order) 
			coeff		      <- 1/self$d

			newpoints     <- as.matrix(x[ordermtx[1,],]) 
			ylrm		      <- y[ordermtx[1,]]
			for(p in 2:(self$d+1)) 
			{
				newpoints   <- newpoints + as.matrix(x[ordermtx[p,],]) 	
				ylrm		    <- ylrm + y[ordermtx[p,]]
			}
			newpoints 	  <- newpoints/(self$d+1)
			ylrm		      <- ylrm/(self$d+1)

			for(p in 1:(self$d+1)) 
			{
				np 		      <- 2* as.matrix(x[ordermtx[p,],]) 
				yp 		      <- 2* y[ordermtx[p,]]
				for (ii in 1:(self$d+1))
				{ 
					if(ii != p) 
					{
						np   	  <- np + coeff*as.matrix(x[ordermtx[ii,],]) 	
						yp		  <- yp + coeff*y[ordermtx[ii,]]
					}
				}
				np 	 		    <- np/3
				yp			    <- yp/3
				newpoints 	<- rbind(newpoints, np)
				ylrm 		    <- rbind(ylrm, yp)
			}
			return( list(Xlrm= newpoints, Ylrm= ylrm ))			
		}
	)
)
