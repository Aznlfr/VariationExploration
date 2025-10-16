library(shellpipes)
library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(deSolve)
loadEnvironments()
B0 <- c(1.5,5,8)
alpha<-1
steps<-3e3
kpa<-0
tau <- pi*2
omega<-tau/60
cars<-1
sigma<-c(0.1)
finTime<-100
y0 <-1e-9
t0 <-100
cohortProp <- 0.6
xlimits<-t0 + finTime*cohortProp
cohorts <- map_dfr(B0, function(B0){
  return(data.frame(sapply(cohortStatsRcPlot(kpa = kpa
                                             , omega=omega
                                             , B0=B0
                                             , alpha = alpha
                                             , steps=steps
                                             , cars = cars
                                             , sigma = sigma
                                             , finTime = finTime
                                             , y0 = y0
                                             , t0=t0
                                             , cohortProp = cohortProp
                                             ),
                           unlist), B0 = B0))
}
)

straightSim <- map_dfr(B0, function(B0){
  return(data.frame(sim(kpa = kpa, omega=omega, B0=B0, alpha=alpha, 
                        timeStep=finTime/steps,
                        finTime=finTime,  cars=cars,
                        sigma=sigma, y0 = 1e-9, t0 = t0
  ), B0 = B0))
}
)


saveEnvironment()
