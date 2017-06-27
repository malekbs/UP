#'  A UPClass is a class the contains a resampling technique and computes the weights of the UP distirbution
#' 
#' @description \code{UPClass} is a class for the resampling technique and its use in the UP disitrbution paradigm.
#' 
#' @details It scale the data, initilize a resmapling technique and compute the weigths of the UP distirbution
#'
#' @docType class
#' @importFrom R6 R6Class 
#' @field Resampling: a resampling technqiue (Default leva-one-out LOO)
#' @field x : design points 
#' @field y : response
#' @field Xscaled  : Scaled design points 
#' @field Yscaled : Sacelde response
#' @field d  : dimension 
#' @field n : number of design points
#' @field rho: weight parameters
#' @field variogramme: weight paramter (default "gauss")
#'
#' @section Methods: 
#' \describe{
#'   \item{\code{UPClass$new(x = NA, y = NA, Scale =TRUE,
#'					resampling_type = "LOO", ListIndices = NULL, 
#'					kfold = 10, division_method ="random") }}{Creates a new \code{UPClass} object. }
#'   \item{\code{scaleinput(newData)}}{ Scale new data.}
#'   \item{\code{unscaleoutput(values )}}{unscale scaled output}
#'	 \item{\code{computeweights(scnewdata)}}{ compute the up weights for a new set of data.}
#' }
#' 			
#' @usage # up     <- UPClass$new()
#'
#' @examples
#' library(UP)
#' d          	<- 2;
#' n           	<- 16
#' X    		    <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
#' y          	<- apply(X, 1, branin)
#' aUP        	<- UPClass$new(X,y,Scale =TRUE, resampling_type = "KFCV", kfold =5)
#' 
#'    
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom pracma pdist2

UPClass <- R6Class("UPClass",
        public = list(
				## Resampling class
				Resampling 			= NULL,
				## data 
				x 					= NULL,
				y 					= NULL,
				Xscaled				= NULL,
				Yscaled				= NULL,
				n					= 0,
				d					= 0,
				## scale Values (input)
				Scale 				= TRUE,
				resampling_type		= "LOO",
				ListIndices			= NULL,
				kfold				= 10,
				division_method		= "random",
				###############################
				##   Resampling Values 		 ##
				##   default initialization  ##
				###############################
				#resampling_type	 	= "LOO", 
				#kfold			 	= 10,
				#division_method		= "random",
				
				## Resampling technique 
				## weights attributs
				rho					= -1,
				variogramme			= "gauss",
				
				################################
				## 			constructor
				###############################
				initialize = function(x = NA, y = NA, Scale =TRUE,
					resampling_type = "LOO", ListIndices = NULL, 
					kfold = 10, division_method ="random") {
					
          self$Scale						<- Scale
					self$resampling_type			<- resampling_type
					self$ListIndices				<- ListIndices
					self$kfold						<- kfold
					self$division_method			<- division_method
					self$setDOE(x,y)
				},
				setDOE =  function(x,y) { 
					self$d 							<- ncol(x)
					self$n							<- nrow(x)
					self$x 							<- x
					self$y 							<- y
					if (!self$Scale)
					{
						private$dOutSlope			<- 1
						private$dOutIntercept		<- 0
						private$vdScaleSlope		<- rep(1,self$d) 
						private$vdScaleIntercept	<- rep(0,self$d) 

						self$Xscaled 				<- self$x 	
						self$Yscaled 				<- self$y
					}
					else
					{
						## Scale Variables (inputs)
						private$vdMaxima			<- apply(x,2,max)
						private$vdMinima			<- apply(x,2,min)
						private$vdScaleSlope		<- 1/(private$vdMaxima-private$vdMinima)
						private$vdScaleIntercept	<- -private$vdMinima*private$vdScaleSlope 
						## scaled DOE
						Xscaled						<- t(t(x)*private$vdScaleSlope  + private$vdScaleIntercept )
						self$Xscaled 				<- Xscaled	
				 
						## Add tests for flate functions
						## scale outputs
						private$dOutMax 			<- drop(max(y))
						private$dOutMin				<- drop(min(y))

						private$dOutSlope			<- 1/(private$dOutMax-private$dOutMin) 
						private$dOutIntercept		<- -private$dOutMin*private$dOutSlope
						self$Yscaled 				<-  y*private$dOutSlope + private$dOutIntercept
							 
					}
					
					self$Resampling					<- Resampling$new(x =self$Xscaled , y = self$YScaled,
													resampling_type = self$resampling_type,  ListIndices = self$ListIndices, 
													kfold = self$kfold, division_method = self$division_method) 

					distmt 						<- dist(Xscaled)
					distmtx 					<- as.matrix(distmt) + 10*diag(self$d,self$n)
					minimumdistances  			<- apply(distmtx,1,min)
					self$rho					<- max(minimumdistances) 
					
				},
				set_weightrho = function(rho) {
				  self$rho <- rho
				},
				
				set_Resampling = function(val) {
				  self$Resampling <- Resampling
				},
				set_ResamplingbyList = function(ListIndices,name= "user_defined_resampling") {
				  self$Resampling <- Resampling$new(x =self$Xscaled , y = self$YScaled, 
										ListIndices= ListIndices, name=name )
				},
				show = function() {
					self$Resampling$show()
					cat(paste0(" variogramme for weights ", self$variogramme ,".\n" ))
					cat(paste0(" rho ", self$rho ,".\n" ))
				},
				scaleinput = function(newData){	
					return(t(t(newData)*private$vdScaleSlope + private$vdScaleIntercept))
				},	 
				unscaleoutput = function(values){	
					return((values-private$dOutIntercept)/private$dOutSlope)
				},
				scaleoutput = function(values){
				  return((values*private$dOutSlope + private$dOutIntercept))
				},
				get_numpoints = function() {
					return(self$n)
				},
				get_dimension = function(){
				return (self$d)
				},
				get_DOE = function() {
				return (list( x=self$x, y= self$y))
				},
				computeweights = function(scnewdata)
				{ 
					################################
					## 			TO DO
					################################
					if(self$Resampling$resampling_type=="LOO")
					{
						distMtx 			<- pdist2(as.matrix(scnewdata), self$Xscaled)
					}
					else
					{
						nbPoints 			<- nrow(scnewdata)
						nbFolds 			<- length(self$Resampling$ListIndices)
						distMtx				<- matrix(rep(0,nbPoints*nbFolds ), nrow=nbPoints, ncol = nbFolds)
						for (i in 1:nbFolds) 
						{
							distMtxtemp 	<- pdist2(as.matrix(scnewdata),
													self$Xscaled[self$Resampling$ListIndices[[i]],])
							distMtx[,i]		<- apply(distMtxtemp,1,min) 
						}			
					}
					coeff 					<- 1 - (exp(-10*distMtx*distMtx/self$rho))
					mindist					<- apply(distMtx,1,min)
					return (list(coeff = coeff, mindist=mindist)) 
				}
			  ), 
			  private = list(
				vdMaxima			= NULL,
				vdMinima			= NULL,
				vdScaleSlope		= NULL,
				vdScaleIntercept	= NULL,
				## scale Values (output)
				dOutMax 			= NULL,	
				dOutMin				= NULL,	
				dOutSlope			= NULL,
				dOutIntercept		= NULL
				)
				
)
