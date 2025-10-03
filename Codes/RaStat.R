
mRaderivs <- function(time, vars, parms){
  dens <- with(parms,
               cars^cars*(time - T0)^(cars-1)*exp(-cars*(time -
                                                           T0))/factorial(cars-1))
  Rc <- parms$Ri*(time - parms$T0)
  return(with(c(parms, vars), list(c(
    # Rcdot = parms$Ri
      ddot = dens
    , Rctotdot = Rc*dens
    , RcSSdot = Rc*Rc*dens
  ))))
}

cRaMoments <- function(time, Ri, T0, cars){
  #	mfuns$sfun <- sfun
  mom <- as.data.frame(ode(
    y=c(cumden=0, Rctot=0, RcSS=0)
    , func=mRaderivs
    , times=time
    , parms=list(
      T0=T0, cars=cars, Ri = Ri)
  ))
  return(mom)
}


RaCCalc <- function(sdat, cohort, ST, tol=1e-4, cars, omega, B0, B1, kpa){
  with(sdat, { 
    ST0<- ST^(kpa + 1)
    BT0 <- B0*exp(B1*sin(omega*cohort))
    sTime <- time[time>=cohort]
    mom <- cRaMoments(sTime, Ri = BT0*ST0, T0=cohort, cars = cars)
    with(mom[nrow(mom), ], {
      stopifnot(abs(cumden-1)<tol)
      Rctot=Rctot/cumden
      RcSS=RcSS/cumden
      return(list(
        cohort=cohort, Rc=Rctot, varRc=(RcSS-Rctot^2), RcSS =RcSS
      ))
    })
  })
}
RaCohortStats <- function(kpa = 0
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
    sapply(cohorts, function(c) RaCCalc(sdat = sdat, cohort=c, ST=sfun(c), tol=1e-4,
                                        cars=cars,
                                        omega = omega,
                                        B0 = B0,
                                        B1 = B1,
                                        kpa = kpa
    ))
  )))
}


outbreakStatsRa <- function(kpa = 0
                            , omega=0
                            , B0=1
                            , B1 = 0
                            , y0=1e-3
                            #  , tmult=6
                            , cohortProp=0.6
                            , steps=300
                            , dfun = boxcar
                            , cars = 1
                            , gmma = 0
                            , finTime = 365
){
  mySim<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
              y0 = y0, finTime=finTime, dfun=dfun, cars=cars, gmma=gmma
  )
  with(mySim, {
    ifun <- approxfun(time, y*x^(kpa + 1), rule=2)
    cStats <- RaCohortStats(kpa = kpa, omega = omega, B0 = B0, B1=B1,
                            sdat=mySim,
                            maxCohort=cohortProp*finTime, 
                            cars=cars)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    varrcfun <- approxfun(cStats$cohort, cStats$varRc, rule=2)
    wssfun <- approxfun(cStats$cohort, cStats$RcSS, rule = 2)
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0, V=0, w = 0, checkV = 0)
      , func=oderivs
      , times=unlist(cStats$cohort)
      , parms=list( B0 = B0, B1=B1, omega=omega,
                    ifun=ifun, rcfun=rcfun, varrcfun=varrcfun,
                    wssfun = wssfun))
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
      return(c(  B0 = B0
                 , B1 = B1
                 , gamma = gmma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , Finalsize=Finalsize
                 , muRa=mu
                 , mu2Ra = mu^2
                 , withinRa=within
                 , checkWithinRa = checkV
                 , betweenRa=between
                 , withinSSRa = w
                 , simplifiedTotalVRa = w - mu^2
                 , totalVRa = total*mu^2
                 , totalKRa = total
                 , simplifiedTotalKRa = otherCheck
      ))
    })
  })
}

