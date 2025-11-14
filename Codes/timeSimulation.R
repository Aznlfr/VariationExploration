library(deSolve)
library(dplyr)
library(patchwork)
library(tidyr)
library(shellpipes)
library(purrr)
loadEnvironments()
#for v1 related to Roswell's paper
straightSim <- map_dfr(B0, function(B0){
  return(data.frame(sim( B0=B0,
                         cars = cars,
                         t0 = t0,
                         timeStep=temporalFinalTime/steps,
                         finTime=temporalFinalTime+2,
                         y0 = y0
  ), B0 = B0))
}
)


saveEnvironment()

