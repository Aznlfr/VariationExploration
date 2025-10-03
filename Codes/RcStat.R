
library(deSolve)

## m for moment; these two functions integrate across the infectors from a given cohort
mderivs <- function(time, vars, parms){
  Bt<- parms$plist$B0*exp(parms$plist$B1*sin(parms$plist$omega*time))
  Sp<-parms$flist$sfun(time)
	Ri <- Bt*Sp*Sp^parms$plist$kpa
	dens <- with(parms$plist,
	             cars^cars*(time - T0)^(cars-1)*exp(-cars*(time - T0))/factorial(cars-1))
	return(with(c(parms, vars), list(c(
	    Rcdot = Ri
		, ddot = dens
		, Rctotdot = Rc*dens
		, RcSSdot = Rc*Rc*dens
		, RcSSSdot = Rc*Rc*Rc*dens
		, RcS4dot = Rc*Rc*Rc*Rc*dens
	))))
}
cMoments <- function(time, sfun, T0, cars, omega, B1, kpa, B0){
#	mfuns$sfun <- sfun
	mom <- as.data.frame(ode(
		y=c(Rc=0, cumden=0, Rctot=0, RcSS=0, RcSSS=0, RcS4 = 0)
		, func=mderivs
		, times=time
		, parms=list(
			plist=list(T0=T0, cars=cars, omega=omega, B1=B1, kpa = kpa, B0=B0)
			, flist=list(sfun=sfun)
		)
	))
	return(mom)
}
cCalc <- function(sdat, cohort, sfun, tol=1e-4, cars, omega, B0, B1, kpa){
  with(sdat, {
    Bcohort<-B0*exp(B1*sin(omega*cohort))
    Ri <- Bcohort*sfun(cohort)^(kpa + 1)
    sTime <- time[time>=cohort]
    mom <- cMoments(sTime, sfun, T0=cohort, cars = cars, omega=omega,
                    B1=B1, kpa=kpa, B0=B0)
    with(mom[nrow(mom), ], {
      #stopifnot(abs(cumden-1)<tol)
      Rctot=Rctot/cumden
      RcSS=RcSS/cumden
      RcSSS=RcSSS/cumden
      RcS4=RcS4/cumden
      return(list(
        cohort=cohort, Ri = Ri, Rc=Rctot, varRc=(RcSS-Rctot^2), RcSS =RcSS,
        RcSSS = RcSSS, RcS4=RcS4
      ))
    })
  })
}

cohortStats <- function(kpa = 0
                        , omega = 0
                        , B0 = 1
                        , B1 = 0
                        , sdat = NULL
                        , maxCohort = NULL
                        , cohortProp=0.6
                        , dfun = boxcar
                        , cars = 1
                        , ...){
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) cCalc(sdat = sdat, cohort=c, sfun=sfun, tol=1e-4,
                                      cars=cars,
                                      omega = omega,
                                      B0 = B0,
                                      B1 = B1,
                                      kpa = kpa
    ))
  )))
}
outbreakStats <- function(kpa = 0
                          , omega=0
                          , B0=1
                          , B1 = 0
                          #  , tmult=6
                          , cohortProp=0.6
                          , steps=300
                          , dfun = boxcar
                          , cars = 1
                          , gmma = 0
                          , finTime = 365
                          , y0 = 1e-9
                          , t0 = 0){
  mySim<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars, gmma=gmma, y0 =y0, t0=t0
  )
  with(mySim, {
    maxCohort <- t0 + cohortProp*finTime
    ifun <- approxfun(time, y*x^(kpa + 1), rule=2)
    cStats <- cohortStats(kpa = kpa, omega = omega, B0 = B0, B1=B1,
                          sdat=mySim,
                          maxCohort=maxCohort, 
                          cars=cars)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    rifun <- approxfun(cStats$cohort, cStats$Ri, rule=2)
    varrcfun <- approxfun(cStats$cohort, cStats$varRc, rule=2)
    wssfun <- approxfun(cStats$cohort, cStats$RcSS, rule = 2)
    wsssfun <- approxfun(cStats$cohort, cStats$RcSSS, rule = 2)
    ws4fun <- approxfun(cStats$cohort, cStats$RcS4, rule = 2)
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0, V=0, w = 0, checkV = 0, 
          Ri = 0, muRi = 0, SSRi = 0, SSS=0, S4 = 0)
      , func=oderivs
      , times=unlist(cStats$cohort)
      , parms=list( B0 = B0, B1=B1, omega=omega,
                    ifun=ifun, rcfun=rcfun, varrcfun=varrcfun,
                    wssfun = wssfun, rifun=rifun, 
                    wsssfun = wsssfun, ws4fun = ws4fun))
    )
    
    with(mom[nrow(mom), ], {
      mu <- mu/finS
      SS <- SS/finS
      w <- w/finS
      checkV <- (checkV/finS)/mu^2
      within <- (V/finS)/mu^2
      between <- (SS-mu^2)/mu^2
      total = within + between
      otherCheck = (w-mu^2)/mu^2
      Finalsize <- finS
      muRi <- muRi/finS
      SSRi <- SSRi/finS
      thirdRc <- SSS/finS
      fourthRc <- S4/finS
      totalVRi <- SSRi - muRi^2
      return(c(  stepSize=steps
                 , B0 = B0
                 , B1 = B1
                 , gamma = gmma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , Finalsize=Finalsize
                 , muRc=mu
                 , within=within
                 , checkWithin = checkV
                 , between=between
                 , withinSS = w
                 , totalVRc = total*mu^2
                 , totalKRc=total
                 , thirdRawRc = thirdRc 
                 , fouthRawRc = fourthRc 
                 , totalVRi=totalVRi
                 , muRi = muRi
                 , timeIntegralRi = Ri
      ))
    })
  })
}
oderivs <- function(time, vars, parms){
  Bt<-parms$B0*exp(parms$B1*sin(parms$omega*(time)))
  inc <- Bt*parms$ifun(time)
  Rc <- parms$rcfun(time)
  Ri <- parms$rifun(time)
  varRc <- parms$varrcfun(time)
  wss <- parms$wssfun(time)
  wsss <- parms$wsssfun(time)
  ws4<- parms$ws4fun(time)
  return(list(c(
    finSdot = inc
    , mudot = inc*Rc
    , SSdot = inc*Rc*Rc
    , Vdot = inc*varRc
    , wdot = inc*wss
    , checkVdot = inc*(wss - Rc^2)
    , Ridot = Ri
    , muRidot = inc*Ri
    , SSRidot = inc*Ri*Ri
    , SSSdot = inc*wsss
    , S4dot = inc*ws4
    
  )))
}
boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0*exp(B1*sin(omega*time))
    xdot <- -Bt*y*x*x^kpa + cars*gmma*r
    ydots[[1]] <- Bt*y*x*x^kpa - cars*yvec[[1]]
    cumdot <- Bt*y*x*x^kpa
    if (cars > 1) {
      ydots[2:cars] <- cars * (yvec[1:(cars - 1)] - yvec[2:cars])
    }
    rdot <- cars*yvec[[cars]] - cars*gmma*r
    out <- c(xdot, ydots, rdot, cumdot)
    names(out) <- c("xdot", paste0("y", 1:cars, "dot"), "rdot", "cumdot")
    return(list(out))
  }
  )
}
sim <- function(kpa = 0, B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar, B1=0, omega=0, gmma=0, t0 =0, 
                y0 = 1e-9){
  x0 <- 1-y0
  r0 <- 0
  cum0 <- 0
  infc <- rep(y0/cars,cars) # y0 is distributed over all infectious cars
  names(infc) <- paste0("y", 1:cars)
  y_init <- c(x = x0, infc, r=r0, cum = cum0)
  if(t0 !=0) timePoints<- c(0, seq(from=t0, to=t0 + finTime, by=timeStep))
  else timePoints<- seq(from=t0, to=t0 + finTime, by=timeStep)
  print(paste0("min",min(timePoints),"max",max(timePoints)))
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=timePoints
    , parms=list(omega=omega, B0=B0, B1 = B1,  cars = cars,
                 kpa = kpa, gmma=gmma)
  ))
  if(t0!=0){sim <- sim[!sim$time==0,]}
  return(within(sim, {
    if (cars>1){
      y <- rowSums(as.data.frame(mget(paste0("y", 1:cars))))
    }
    else{
      y <- y1
    }
  }))
}

cohortStatsRcPlot <- function(kpa = 0
                              , omega=0
                              , B0=1
                              , B1 = 0
                              , cohortProp=0.6
                              , steps=300
                              , dfun = boxcar
                              , cars = 1
                              , gmma = 0
                              , finTime = 365
                              , y0 = 1e-9
                              , t0 = 0
                              
){
  sdat<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
             finTime=finTime, dfun=dfun, cars=cars, gmma=gmma, y0=y0, t0=t0
  )
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  maxCohort <- t0 + cohortProp * finTime
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) cCalc(sdat = sdat, cohort=c, sfun=sfun, 
                                      tol=1e-4,
                                      cars=cars,
                                      omega = omega,
                                      B0 = B0,
                                      B1 = B1,
                                      kpa = kpa
    ))
  )))
}

simwrap <- function(kpa = 0, B0=1,  cars = 1, finTime=365,
                    timeStep=0.1, dfun=boxcar, B1=0, omega=0, gmma=0, y0 =1e-9,
                    t0 = 0){
  sdat<-sim(kpa = kpa, B0 =B0, cars=cars, finTime=finTime, timeStep = timeStep,
            dfun=dfun, B1=B1, omega=omega, gmma = gmma, y0=1e-9, t0=t0)
  return(
    list(
      sdat = sdat, finTime = finTime
    )
  )
}