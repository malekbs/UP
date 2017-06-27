library("UP")
library("lhs")

d           <- 2
n           <- 10
X           <- optimumLHS(n,d)
Y           <- apply(X, 1, branin)
upsm        <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE)) 

plotContour2D(branin,40,40)
lines(x =X[,1], y = X[,2], type = "p",pch=15,col = "blue")
nsteps      <- 10
upego_res   <- upego(upsm, fun = branin, nsteps = nsteps, lower= c(0,0),upper = c(1,1))

lines(upego_res$last$get_DOE()$x[(n+1):(n+nsteps),1],
      upego_res$last$get_DOE()$x[(n+1):(nsteps+n),2],
      "p", pch=15, col = "red")
