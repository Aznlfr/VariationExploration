library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

year <- 60
tau <- 2*pi
omega <- c(0) 
B0 <- c(1.5,3.5,5,8)
alpha<- c(0)
steps<- 5e3
cars <- c(1) #number of compartments
kpas <- 0:3 #the variation in the susceptibility
sigma <- c(0) # waning immunity rate
t0<-0
finTime<-100
y0<-1e-9
grid <- expand.grid(B0=B0, kpas=kpas)
system.time({
res_mat <- as.data.frame(t(mapply(outbreakStats, B0=grid$B0,
                                  kpa=grid$kpas,
                                  MoreArgs = list(alpha=alpha,
                                                  omega=omega,
                                                  cars=cars,
                                                  sigma=sigma,
                                                  finTime=finTime,
                                                  steps=steps,
                                                  y0=y0,
                                                  t0=t0))))
})

saveEnvironment()
