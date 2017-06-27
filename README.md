# UP : Universal Predicition Package

a package that provides universal method for surrogate models: A package with extensible options and various 
UP-based algorithms for optimization, refienment and inversion.


Installation
------------

You can install the latest version of the code using the `devtools` R package.

```{r}
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("malekbs/UP")
```

Usage
-----

```
d           <- 2;
n           <- 16
testdata    <- expand.grid(x1=s <- seq(0,1, length=5), x2=s)
design.fact <- expand.grid(x1=seq(0,1,length=5), x2=seq(0,1,length=4))
y           <- apply(design.fact, 1, branin)
aUP         <- UPClass$new(design.fact,y,Scale =TRUE, resampling_type = "KFCV", kfold =5)
kriging     <- krigingsm$new()
upkrig      <- UPSM$new(sm= kriging, UP =aUP)
predicted.values.model1 <- upkrig$uppredict(testdata)
max(predicted.values.model1$upsd)
upkrig2     <- UPSM$new(sm= krigingsm$new(), UP=UPClass$new(design.fact,y,Scale =TRUE))
uppred2     <- upkrig2$uppredict(testdata)
max(uppred2$upsd)
upsvm       <- UPSM$new(sm= svmsm$new(), UP=UPClass$new(design.fact,y,Scale =TRUE))
uppred3     <- upsvm$uppredict(testdata)
```


References
---------

BEN SALEM, M., ROUSTANT, O., GAMBOA, F. and TOMASO, L. (2015). Universal Prediction distribution for surrogate models. arXiv preprint arXiv:1512.07560.

Roustant, O., Ginsbourger, D. and Deville, Y., 2012. DiceKriging, DiceOptim: Two R Packages for the Analysis of Computer Experiments by Kriging-Based Metamodeling and Optimization. Journal of statistical software, 51(1), pp.1-55.
