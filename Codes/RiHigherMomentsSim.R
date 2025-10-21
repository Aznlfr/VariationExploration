library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

#Simple SIR model with fixed B0
B0 <- c(2,4,7,9,10)
steps<- 3e3
t0<-0
finTime<-100
y0<-1e-9
grid <- expand.grid(B0=B0)
res_mat <- as.data.frame(t(mapply(RioutbreakStats, B0=grid$B0,
                                     MoreArgs = list(
                                                  finTime=finTime,
                                                  steps=steps,
                                                  y0=y0,
                                                  t0=t0))))

saveEnvironment()
