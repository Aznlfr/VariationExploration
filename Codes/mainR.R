source("RcStat.R")
library(xtable)
# beta(t) = B0 exp(B1 sin(omega t))
omega <- c(pi/30) 
B0 <- c(1.5,2,4)
B1<- c(1)
steps<- 2e3
cars <- c(1) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
gmma <- 1# waning immunity rate
grid <- expand.grid(omg = omega, B0=B0)

res_mat <- as.data.frame(t(mapply(outbreakStats, omega= grid$omg, B0=grid$B0,
                     MoreArgs = list(B1 = B1,  gmma=gmma,
                           cars=cars, kpa=kpas, steps=steps,
                           t0=t0, finTime = 150
                           ))))

xtable(res_mat, row.names=FALSE)



