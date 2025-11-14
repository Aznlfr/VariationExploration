library(dplyr)
library(purrr)
library(tidyr)
library(deSolve)
library(shellpipes)
#An attempt to implement V1 of Roswell's manuscript using a deterministic framework
loadEnvironments()

  res_mat <- map_dfr(B0, function(x){v1Stats_s(B0 = x
                                               ,cars=cars
                                               ,cohortProp=cohortProp
                                               ,steps=steps
                                              ,y0=y0
                                              ,cutoffTime = cutoffTime
                                              ,finTime = finTime
                                              ,t0=t0)
})


saveEnvironment()
