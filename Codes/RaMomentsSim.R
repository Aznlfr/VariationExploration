library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

B0 <- c(2,4)
alpha <- 0
omega <- 0
sigma <- 0
kpa <- 0
cars <- 1
steps<- 3e3
t0<-0
finTime<-100
y0<-1e-9
grid <- expand.grid(B0=B0)
res_mat <- as.data.frame(t(mapply(outbreakStatsRa, B0=grid$B0,
                                     MoreArgs = list(
                                                  alpha = alpha,
                                                  omega = omega,
                                                  sigma = sigma,
                                                  kpa = kpa,
                                                  cars = cars,
                                                  finTime=finTime,
                                                  steps=steps,
                                                  y0=y0,
                                                  t0=t0))))

saveEnvironment()
