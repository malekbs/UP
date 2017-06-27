library(UP)

x               <- as.matrix(c(-3,-2.6,-0.2, 1.7,-1.4,1.2,3))
y               <- sin(x)
xverif          <- seq(-3, 3, length.out =300)

###### first metamodel kriging ######
krig            <- krigingsm$new()
resampling      <- UPClass$new(x, y, Scale = TRUE)
upsm            <- UPSM$new(sm= krig, UP= resampling)
prediction      <- upsm$uppredict(xverif)
plotUP1D(xverif, prediction, x, y)

###### SVM accroding to different cost value #####
upsvm           <- UPSM$new(sm = svmsm$new(parameters = list(cost=7)), UP= resampling)
prediction2     <- upsvm$uppredict(as.matrix(xverif))
plotUP1D(xverif, prediction2, x, y)

######### aggregation ############################ 

######### fitness function ####################### 
listCrit        <- list()
listCrit[[1]]   <- MSE$new()
listCrit[[2]]   <- Resampling_Error$new()
listCrit[[3]]   <- penlrm$new(x, y)
fit             <- custom_fit$new(c(4,2,1), listCrit)

listsm          <- list()
listsm[[1]]     <- upsm
listsm[[2]]     <- upsvm
parameters      <- list(listsm =listsm)
ens             <- aggregation$new(fit, x, y, parameters = parameters)
ens$train()
prediEns        <- ens$predict(xverif)

prediction_ens  <- ens$uppredict(as.matrix(xverif))
plotUP1D(xverif, prediction_ens, x, y)

