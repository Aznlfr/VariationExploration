library(deSolve)
library(shellpipes)
rpcall("Codes/RiStat.Rout Codes/RiStat.R")

loadEnvironments()

RioutbreakStats <- function(
                            B0=1
                          , cohortProp=0.6
                          , steps=300
                          , dfun = boxcar
                          , finTime = 365
                          , y0 = 1e-9
                          , t0 = 0){
  mySim<- sim(B0=B0, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun,  y0=y0, t0=t0
  )
  with(mySim, {
    maxCohort <- t0 + cohortProp*finTime
    ifun <- approxfun(time, B0*y*x, rule=2)
    rifun <- approxfun(time, B0*x, rule=2)
    mom <- as.data.frame(ode(
      y=c(finS=0,  
          muRi = 0, SSRi = 0,  SSSRi = 0, S4Ri=0)
      , func=odeRi
      , times=time
      , parms=list(
                    ifun=ifun,  rifun=rifun))
    )
    
    with(mom[nrow(mom), ], {
      muRi <- muRi/finS
      SSRi <- SSRi/finS
      SSSRi <- SSSRi/finS
      S4Ri <- S4Ri/finS
      totalVRi <- SSRi - muRi^2
      return(c(  stepSize=steps
                 , B0 = B0
                 , alpha = 0
                 , sigma = 0
                 , omega = 0
                 , cars = 1
                 , kpa = 0
                 , muRi = muRi
                 , totalVRi=totalVRi
                 , thirdRawRi = SSSRi
                 , fourthRawRi = S4Ri
      ))
    })
  })
}

odeRi <- function(time, vars, parms){
  inc <- parms$ifun(time)
  Ri <- parms$rifun(time)
  return(list(c(   
    inc #finS
    ,inc*Ri #muRi
    ,inc*Ri*Ri #SSRi
    ,inc*Ri*Ri*Ri #SSSRi
    ,inc*Ri*Ri*Ri*Ri #S4Ri
  )))
}
boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    xdot <- -B0*y*x
    ydot <- B0*y*x - y
    cumdot <- B0*y*x
    rdot <- y
    out <- c(xdot, ydot, rdot, cumdot)
    return(list(out))
  }
  )
}
sim <- function(B0=1, finTime=365,
                timeStep=0.1, dfun=boxcar,  t0 =0, 
                y0 = 1e-9){
  x0 <- 1-y0
  r0 <- 0
  cum0 <- 0
  y_init <- c(x = x0, y=y0, r=r0, cum = cum0)
  if(t0 !=0) timePoints<- c(0, seq(from=t0, to=t0 + finTime, by=timeStep))
  else timePoints<- seq(from=t0, to=t0 + finTime, by=timeStep)
  print(paste0("min",min(timePoints),"max",max(timePoints)))
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=timePoints
    , parms=list(B0=B0)
  ))
  if(t0!=0){sim <- sim[!sim$time==0,]}
  return(within(sim, {
    inc <- c(diff(cum),0)
    
  }))
}
saveEnvironment()