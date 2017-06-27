 sm <- R6Class("sm", 
		public = list(
		########################
		### Surrogate model
		########################
		model  		= NULL,
		#######################
		## Resampling UP method
		#######################
		parameters	= NULL,
		x			= NULL,
		y 			= NULL,
		d						= 0,
		n						= 0,
		
		fitness		= 1e10,
		
		readytopredict = FALSE,
		fast_computation = FALSE,
		resampling_method = "",
		resamplingparam   = NULL,
		initialize= function(x,y) 
		{
			self$model  		= NULL
			self$parameters		= NULL

			self$x				= x
			self$y 				= y
			
			self$fitness		= 1e10
			
			self$readytopredict = FALSE
		},
		setDOE = function(x,y) {
				self$x				= x
				self$y 				= y

				self$d= ncol(self$x)
				self$n= nrow(self$x)
				self$readytopredict = FALSE
			},
		train= function() 
		{
			if ( is.null( self$x)|| is.null(self$y)) 
			{
				stop(" The design of experiment is missing")
			}
			self$readytopredict = TRUE
		},
		
		predict= function(newdata) 
		{
			return(NULL)
		},
		submodelspredictions= function() 
		{
			return(NULL)
		},
		get_numpoints = function() {
				return (self$n)
		},
		get_dimension = function() {
			return (self$d)
		},
		
		get_DOE = function() {
				return (list( x=self$x, y= self$y))
		},
				
		supportFastComputation= function(Resampling) {
			self$resampling_method <- Resampling$resampling_type
			return(self$fast_computation)
		}
	) 
)
