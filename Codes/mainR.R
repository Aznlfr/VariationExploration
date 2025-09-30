source("CommonFs.R")
source("RcStat.R")
source("RiStat.R")
library(xtable)
# beta(t) = B0 exp(B1 sin(omega t))
omega <- c(0, pi/30, pi/20) 
B0 <- c(2)
B1<- c(0.5,0.6)
steps<- 2e3
cars <- c(1) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
gmma <- 0.5 # waning immunity rate

grid <- expand.grid(omg = omega, B1=B1)

res_mat <- as.data.frame(t(mapply(outbreakStats, omega= grid$omg, B1=grid$B1,
                     MoreArgs = 
                      list(B0 = B0, y0 = 1e-9, gmma=gmma,
                           cars=cars, kpa=kpas, steps=steps))))

xtable(SeasonDat, row.names=FALSE)


