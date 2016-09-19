
library(snow)
z=vector('list',4)
z=1:4
system.time(lapply(z,function(x) Sys.sleep(1)))
cl<-makeCluster(4,type="SOCK")
system.time(clusterApply(cl, z,function(x) Sys.sleep(1)))
stopCluster(cl)


library(parallel)
detectCores()

library(doSNOW)

NumberofCluster <- 4 

cl <- makeCluster(NumberofCluster)
registerDoSNOW(NumberofCluster)
stopCluster(cl)
