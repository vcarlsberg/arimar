#coba gridsearch

library(NMOF)
testFun <- function(x)
  x[1] + x[2]^2

sol <- gridSearch(fun = testFun, levels = list(1:2, c(2, 3, 5)))
sol$minfun
sol$minlevels

levels <- list(a = 1:2, b = 1:3)
res <- gridSearch(testFun, levels)
res$minfun
res$minlevels
