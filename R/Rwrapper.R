#' Traditional R calls
#' 
#' \code{kriging} - kriging surrogate model
#' @param x input values
#' @param y output values
#' @param parameters kriging parameters
#' @param type kriging type
#' 
#' @return kriging: kriging surrogate model.
#' @rdname Rwrapper
#' @export 
kriging <- function( x = NULL,y = NULL,parameters = NULL, type= "UK") 
{
  return(krigingsm$new(x=x, y=y, parameters=parameters, type=type))
}

#' Traditional R calls
#' 
#' \code{svr} - suport vector machine regression surrogate models
#' 
#' @return svr: svr surrogate model
#' @rdname Rwrapper
#' @export 
svr <- function(x = NULL, y = NULL, parameters = NULL) 
{
  return(svmsm$new(x=x,y=y,parameters=parameters))
}

#' Traditional R calls
#' 
#' \code{train} - train surrogate models
#' 
#' @param model the surrogate model
#' 
#' @return train: trained surrogate model
#' @rdname Rwrapper
#' @export 
train <- function(model) 
{
  model$train()
  return(model)
}

#' Traditional R calls
#' 
#' \code{predict} - prediction values of the  surrogate models
#' 
#' @param newdata data points where to predict
#' 
#' @return predict: surrogate model predictions
#' @rdname Rwrapper
#' @export 
predict <- function(model, newdata) 
{
  return(model$predict(newdata))
}

#' Traditional R calls
#' 
#' \code{upsm} - create a UP surrogate model
#' 
#' @param sm surrogate model
#' @param UP Resampling method defining the Universal Prediction
#' 
#' @return upsm: UP surrogate model
#' 
#' @rdname Rwrapper
#' @export 
upsm <- function(sm = NULL, UP=NULL) 
{
  return(UPSM$new(sm=sm, UP=UP))
}

#' Traditional R calls
#' 
#' \code{setDOE} - set DOE for a UP surrogate model
#' 
#' @return setDOE: a UP surrogate model with DOE
#' 
#' @rdname Rwrapper
#' @export 
setDOE <- function(model, x,y) 
{
  model$setDOE(x,y)
  return(model)
}

#' Traditional R calls
#' 
#' \code{masterprediction} - master prediction values of a UP surrogate models
#' 
#' @return masterprediction: surrogate model master predictions
#' @rdname Rwrapper
#' @export 
masterprediction <- function(model, newdata) 
{
  return(model$masterprediction(newdata))
}

#' Traditional R calls
#' 
#' \code{uppredict} - UP prediction values of a UP surrogate models
#' 
#' @return uppredict: surrogate model UP predictions
#' @rdname Rwrapper
#' @export 
uppredict <- function(model, newdata) 
{
  return(model$uppredict(newdata))
}


