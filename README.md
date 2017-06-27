# UP : Universal Predicition distirbution R Package

UP is a package that provides universal method for surrogate models: A package with extensible options and various 
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
library(UP)
x           <- as.matrix(c(-2.6,-0.2, 1.7,-1.4,1.2,3))
y           <- c(0.8, 0.5, 0.1, 0.3, 0, 0.4)
xverif      <- seq(-3, 3, length.out =300)
krig        <- krigingsm$new()
resampling  <- UPClass$new(x, y, Scale =TRUE) 
upsm        <- UPSM$new(sm= krig, UP= resampling) 
prediction  <- upsm$uppredict(xverif)
plotUP1D(xverif, prediction, x, y)
```


References
---------

BEN SALEM, M., ROUSTANT, O., GAMBOA, F. and TOMASO, L. (2015). Universal Prediction distribution for surrogate models. arXiv preprint arXiv:1512.07560.

ROUSTANT, O., GINSBOURGER, D. and DEVILLE, Y., 2012. DiceKriging, DiceOptim: Two R Packages for the Analysis of Computer Experiments by Kriging-Based Metamodeling and Optimization. Journal of statistical software, 51(1), pp.1-55.
