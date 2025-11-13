library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)
#An attempt to implement V1 of Roswell's manuscript in a deterministic framework
loadEnvironments()
year <- 60
tau <- 2*pi
omega <- 0
B0 <- c(2,4,8)
alpha<- c(0)
steps<- 5e3
cars <- c(1) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
sigma <- c(0) # waning immunity rate
t0 <- 0 # initial time
finTime <- c(1, 2.93, 6.84, 10, 20.56, 25) # final day of simulation
y0<-1e-9
cohortProp <- 0.6
grid <- expand.grid(B0=B0, finTime=finTime)
system.time({
  res_mat <- as.data.frame(t(mapply(v1Stats,
                                    B0=grid$B0,
                                    finTime=grid$finTime,
                                    MoreArgs = list(alpha=alpha,
                                                    omega=omega,
                                                    cars=cars,
                                                    cohortProp=cohortProp,
                                                    kpa=kpas,
                                                    sigma=sigma,
                                                    steps=steps,
                                                    y0=y0,
                                                    t0=t0))))
})

saveEnvironment()
