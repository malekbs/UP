library("UP")
library("lhs")

d           <- 2
n           <- 30
X           <- optimumLHS(n,d)
Y           <- apply(X, 1, branin)

plotContour2D(branin,30,11)
lines(x =X[,1], y = X[,2], type = "p",pch=15,col = "blue")
nsteps      <- 15
upsm        <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(X,Y,Scale =TRUE))
upinv_res   <- upinverse(upsm, fun = branin, target = 150, 
                  nsteps = nsteps, lower= c(0,0),upper = c(1,1))

lines(upinv_res$last$get_DOE()$x[(n+1):(n+nsteps),1],
      upinv_res$last$get_DOE()$x[(n+1):(nsteps+n),2],
      "p", pch=15, col = "red")

