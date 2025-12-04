library(deSolve)
library(shellpipes)
rpcall("Codes/RcStat.Rout Codes/RcStat.R")
rpcall("Codes/v1Stats.Rout Codes/v1Stats.R Codes/RcStat.Rout Codes/RcStat.rda")

loadEnvironments()
## m for moment; these two functions integrate across the infectors from a given cohort
mderivs <- function(time, vars, parms){
  Bt<- parms$plist$B0
  Sp<-parms$flist$sfun(time) #fraction of susceptible
	Ri <- Bt*Sp
	dens <- with(parms$plist,
	             cars^cars*(time - T0)^(cars-1)*exp(-cars*(time - T0))/factorial(cars-1))
	return(with(c(parms, vars), list(c(
	    Ri
		, dens
		, Rc*dens
		, Rc*Rc*dens
	))))
}

cMoments <- function(time, sfun, T0, cars, B0){
	mom <- as.data.frame(ode(
		y=c(Rc=0, cumden=0, Rctot=0, RcSS=0)
		, func=mderivs
		, times=time
		, parms=list(
			plist=list(T0=T0, cars=cars, B0=B0)
			, flist=list(sfun=sfun)
		)
	))
	return(mom)
}
cCalc <- function(time, cohort, sfun, tol=1e-4, cars, B0){
    Bcohort<-B0
    Ri <- Bcohort*sfun(cohort)
    sTime <- time[time>=cohort]
    mom <- cMoments(sTime, sfun, T0=cohort, cars=cars, 
                     B0=B0)
    with(mom[nrow(mom), ], {
      stopifnot(abs(cumden-1)<tol)
      Rctot=Rctot/cumden
      RcSS=RcSS/cumden
      return(list(
        cohort=cohort, Ri = Ri, Rc=Rctot, varRc=(RcSS-Rctot^2), RcSS =RcSS
      ))
    })
}

cohortStats <- function(B0 = 1
                        , sdat = NULL
                        , maxCohort = NULL
                        , cohortProp=0.6
                        , dfun = boxcar
                        , cars = 1
                        , ...){
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) cCalc(sdat$time, cohort=c, sfun=sfun, tol=1e-4,
                                      cars=cars,
                                      B0 = B0
    ))
  )))
}
v1Stats_s <- function(B0=1
                      , cohortProp=0.6
                      , steps=300
                      , dfun = boxcar
                      , cars = 1
                      , finTime = 365
                      , cutoffTime = NULL
                      , y0 = 1e-9
                      , t0 = 0){
  mySim<- sim(B0=B0, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars,  y0 =y0, t0=t0
  )
  with(mySim, {
    maxCohort <- t0 + cohortProp*finTime
    ifun <- approxfun(time, y*x, rule=2)
    cStats <- cohortStats( B0 = B0,
                           sdat=mySim,
                           maxCohort=maxCohort, 
                           cars=cars)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    varrcfun <- approxfun(cStats$cohort, cStats$varRc, rule=2)
    wssfun <- approxfun(cStats$cohort, cStats$RcSS, rule = 2)
    
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0, V=0, w = 0, checkV = 0)
      , func=v1ODE
      , times=unlist(cStats$cohort)
      , parms=list( B0 = B0, ifun=ifun, rcfun=rcfun, varrcfun=varrcfun,
                    wssfun = wssfun))
    )
    return(((map_dfr(cutoffTime, function(cuttime){
      idx <- which.min(abs(mom$time - cuttime))
      with(mom[idx, ], {
        mu <- mu/finS
        SS <- SS/finS
        w <- w/finS
        checkV <- (checkV/finS)
        within <- (V/finS)
        between <- (SS-mu^2)
        total = within + between
        otherCheck = (w-mu^2)
        Finalsize <- finS
        return(data.frame(stepSize=steps
                   , B0 = B0
                   , finTime=finTime
                   , cutoffTime=cuttime
                   , Finalsize=Finalsize
                   , muRc=mu
                   , within=within
                   , checkWithin = checkV
                   , between=between
                   , withinSS = w
                   , totalVRc = total
                   , totalVRc_simplified = otherCheck
                   , totalKRc=total/mu^2
        ))
      })
    }
    )
)
))
  })}
v1Stats <- function(B0=1
                          , cohortProp=0.6
                          , steps=300
                          , dfun = boxcar
                          , cars = 1
                          , finTime = 365
                          , cutoffTime = 50 
                          , y0 = 1e-9
                          , t0 = 0){
  mySim<- sim(B0=B0, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars,  y0 =y0, t0=t0
  )
  with(mySim, {
    maxCohort <- t0 + cohortProp*finTime
    ifun <- approxfun(time, y*x, rule=2)
    cStats <- cohortStats( B0 = B0,
                          sdat=mySim,
                          maxCohort=maxCohort, 
                          cars=cars)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    varrcfun <- approxfun(cStats$cohort, cStats$varRc, rule=2)
    wssfun <- approxfun(cStats$cohort, cStats$RcSS, rule = 2)
    integrationtime<-with(cStats,{
      a<-unlist(cStats$cohort)
      return(a[a<=cutoffTime])
      } )
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0, V=0, w = 0, checkV = 0)
      , func=v1ODE
      , times=integrationtime
      , parms=list( B0 = B0, ifun=ifun, rcfun=rcfun, varrcfun=varrcfun,
                    wssfun = wssfun))
    )
    
    with(mom[nrow(mom), ], {
      mu <- mu/finS
      SS <- SS/finS
      w <- w/finS
      checkV <- (checkV/finS)
      within <- (V/finS)
      between <- (SS-mu^2)
      total = within + between
      otherCheck = (w-mu^2)
      Finalsize <- finS
      return(c(  stepSize=steps
                 , B0 = B0
                 , finTime=finTime
                 , cutoffTime=cutoffTime
                 , Finalsize=Finalsize
                 , muRc=mu
                 , within=within
                 , checkWithin = checkV
                 , between=between
                 , withinSS = w
                 , totalVRc = total
                 , totalVRc_simplified = otherCheck
                 , totalKRc=total/mu^2
      ))
    })
  })
}

v1ODE <- function(time, vars, parms){
  Bt<-parms$B0
  inc <- Bt*parms$ifun(time)
  Rc <- parms$rcfun(time)
  varRc <- parms$varrcfun(time)
  wss <- parms$wssfun(time)
  return(list(c(  #finS=0, mu=0, SS=0, V=0, w = 0, checkV = 0,     SSS=0, S4 = 0
    inc #finS
    ,inc*Rc #mu
    ,inc*Rc*Rc #RSS
    ,inc*varRc #V
    ,inc*wss #w
    ,inc*(wss - Rc^2) #checkV
  )))
}


boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0
    xdot <- -Bt*y*x 
    ydots[[1]] <- Bt*y*x- cars*yvec[[1]]
    cumdot <- Bt*y*x
    if (cars > 1) {
      ydots[2:cars] <- cars * (yvec[1:(cars - 1)] - yvec[2:cars])
    }
    rdot <- cars*yvec[[cars]] 
    out <- c(xdot, ydots, rdot, cumdot)
    names(out) <- c("xdot", paste0("y", 1:cars, "dot"), "rdot", "cumdot")
    return(list(out))
  }
  )
}
sim <- function(B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar,  t0 =0, 
                y0 = 1e-9){
  x0 <- 1-y0
  r0 <- 0
  cum0 <- 0
  infc <- rep(y0/cars,cars) # y0 is distributed over all infectious cars
  names(infc) <- paste0("y", 1:cars)
  y_init <- c(x = x0, infc, r=r0, cum = cum0)
  if(t0 !=0){timePoints<- c(0, seq(from=t0, to=t0 + finTime, by=timeStep))}else{
      timePoints<- seq(from=t0, to=t0 + finTime, by=timeStep)}
  print(paste0("min",min(timePoints),"max",max(timePoints)))
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=timePoints
    , parms=list(B0=B0,  cars = cars)
  ))
  if(t0!=0){sim <- sim[!sim$time==0,]}
  return(within(sim, {
    if (cars>1){
      y <- rowSums(as.data.frame(mget(paste0("y", 1:cars))))
    }
    else{
      y <- y1
    }
    inc <- c(diff(cum),0)
    
  }))
}

cohortStatsRcPlot <- function( B0=1
                              , cohortProp=0.6
                              , steps=300
                              , dfun = boxcar
                              , cars = 1
                              , finTime = 365
                              , y0 = 1e-9
                              , t0 = 0
                              
){
  sdat<- sim( B0=B0,timeStep=finTime/steps,
             finTime=finTime, dfun=dfun, cars=cars,  y0=y0, t0=t0
  )
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  maxCohort <- t0 + cohortProp * finTime
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) cCalc(sdat$time, cohort=c, sfun=sfun, 
                                      tol=1e-4,
                                      cars=cars,
                                      B0 = B0))
  )))
}




saveEnvironment()
