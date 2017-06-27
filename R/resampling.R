Resampling <- R6Class("Resampling",
                public = list(
				## Resampling technique 
				resampling_type	 	= "LOO", 
				ListIndices		 	= NULL,
				kfold			 	= 10,
				## data 
				x 					= NULL,
				y 					= NULL,
				Xscaled				= NULL,
				Yscaled				= NULL,
				division_method		= "random",
				initialize = function(x = NA, y = NA, resampling_type = "LOO", 
							ListIndices = NULL, kfold = 10, division_method = "random") 
				{
					self$x 					<- as.matrix(x)
					self$y					<- y
					self$resampling_type 	<- resampling_type
					self$ListIndices 		<- ListIndices
					self$kfold 				<- kfold

					self$division_method    <- division_method

					if(resampling_type == "KFCV") 
					{
						if (division_method =="OBO")
						{
							self$ListIndices 		<- OBOSubdivision(x, k=kfold)
							
						}						
						else  
						{
							self$ListIndices		<- RandomSubdivision(x, k=kfold)
						} 
					}
				},
				show = function() {				 
					cat(paste0(" Resampling method: ", self$resampling_type, ".\n"))
				}
			)
)
