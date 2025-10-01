
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
	))))
}
cMoments <- function(time, sfun, T0, cars, omega, B1, kpa, B0){
#	mfuns$sfun <- sfun
	mom <- as.data.frame(ode(
		y=c(Rc=0, cumden=0, Rctot=0, RcSS=0)
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
    sTime <- time[time>=cohort]
    mom <- cMoments(sTime, sfun, T0=cohort, cars = cars, omega=omega,
                    B1=B1, kpa=kpa, B0=B0)
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
                          , yint = NULL
){
  if(is.null(yint)){t0<-0}
  else{t0<-yint$time}
  mySim<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars, gmma=gmma,  yvec0 = yint
  )
  with(mySim, {
    #sdat, cohort, ST, tol=1e-4, cars, omega, B0, B1, kpa
    ifun <- approxfun(time, y*x^(kpa + 1), rule=2)
    cStats <- cohortStats(kpa = kpa, omega = omega, B0 = B0, B1=B1,
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
      , parms=list( B0 = B0, B1=B1, omega=omega, t0=t0,
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
      return(c(  stepSize=steps
                 , B0 = B0
                 , B1 = B1
                 , gamma = gmma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , Finalsize=Finalsize
                 , mu=mu
                 , mu2 = mu^2
                 , within=within
                 , checkWithin = checkV
                 , between=between
                 , withinSS = w
                 , simplifiedTotalV = w - mu^2
                 , totalV = total*mu^2
                 , totalK=total
                 , simplifiedTotalK = otherCheck
      ))
    })
  })
}
