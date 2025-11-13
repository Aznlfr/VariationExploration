library(deSolve)
library(dplyr)
library(patchwork)
library(tidyr)
library(shellpipes)
library(purrr)
loadEnvironments()
#for v1 related to Roswell's paper
temporalFinalTime <- max(finTime)
straightSim <- map_dfr(B0, function(B0){
  return(data.frame(sim( B0=B0,
                         kpa = kpas,
                         cars = cars,
                         alpha = alpha,
                         omega = omega,
                         sigma = sigma,
                         t0 = t0,
                         timeStep=temporalFinalTime/steps,
                         finTime=temporalFinalTime,
                         y0 = 1e-9
  ), B0 = B0))
}
)


saveEnvironment()

