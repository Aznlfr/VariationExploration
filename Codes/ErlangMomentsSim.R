library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

year <- 60
tau <- 2*pi
omega <- c(0) 
B0 <- c(1.5,5,8)
alpha<- c(0)
steps<- 1e4
cars <- c(1,2,4) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
#sigma <- c(0.02, 0.05, 0.1)# waning immunity rate
sigma<-0
t0<-0
finTime<-100
y0<-1e-9
grid <- expand.grid(B0=B0, cars=cars)
res_mat <- as.data.frame(t(mapply(outbreakStats, B0=grid$B0,
                                  cars=grid$cars,
                                  MoreArgs = list(alpha=alpha,
                                                  omega=omega,
                                                  sigma=sigma,
                                                  kpa=kpas,
                                                  finTime=finTime,
                                                  steps=steps,
                                                  y0=y0,
                                                  t0=t0))))

saveEnvironment()