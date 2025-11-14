
library(deSolve)
library(purrr)
library(shellpipes)

loadEnvironments()

cohorts <- map_dfr(B0, function(B0){
  return(data.frame(sapply(cohortStatsRcPlot(B0=B0
                                             , steps=steps
                                             , cars = cars
                                             , finTime = finTime
                                             , y0 = y0
                                             , t0=t0
                                             , cohortProp = cohortProp
  ),
  unlist), B0 = B0))
}
)
saveEnvironment()