library(deSolve)


RiCCalc <- function(cohort, ST, tol=1e-4, cars, omega, B0, B1, kpa, t0){
    ST0<- ST^(kpa + 1)
    BT0 <- B0*exp(B1*sin(omega*(cohort+t0)))
    Rc<-ST0*BT0
      return(list(
        cohort=cohort, Rc=Rc
      ))}
RiCohortStats <- function(kpa = 0
                          , omega = 0
                          , B0 = 1
                          , B1 = 0
                          , sdat = NULL
                          , maxCohort = NULL
                          , cohortProp=0.6
                          , dfun = boxcar
                          , cars = 1
                          , t0 = 0
                          , ...){
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) RiCCalc(cohort=c, ST=sfun(c), tol=1e-4,
                                        cars=cars,
                                        omega = omega,
                                        B0 = B0,
                                        B1 = B1,
                                        kpa = kpa,
                                        t0 = t0
    ))
  )))
}
odeRi <- function(time, vars, parms){
  Bt<-parms$B0*exp(parms$B1*sin(parms$omega*(time + parms$t0)))
  inc <- Bt*parms$ifun(time)
  Rc <- parms$rcfun(time)
  return(list(c(
    finSdot = inc
    , mudot = inc*Rc
    , SSdot = inc*Rc*Rc
  )))
}

outbreakStatsRi <- function(kpa = 0
                            , omega=0
                            , B0=1
                            , B1 = 0
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
              finTime=finTime, dfun=dfun, cars=cars, gmma=gmma, yvec0 = yint
  )
  with(mySim, {
    RiAvg <- Ravg(sdat = mySim, B0 =B0, B1=B1, kpa=kpa, omega=omega,
                  tlag=t0)
    ifun <- approxfun(time, y*x^(kpa + 1), rule=2)
    cStats <- RiCohortStats(kpa = kpa, omega = omega, B0 = B0, B1=B1,
                            sdat=mySim,
                            maxCohort=cohortProp*finTime, 
                            cars=cars,
                            t0 = t0)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0)
      , func=odeRi
      , times=unlist(cStats$cohort)
      , parms=list( B0 = B0, B1=B1, omega=omega, t0 = t0,
                    ifun=ifun, rcfun=rcfun))
    )
    
    with(mom[nrow(mom), ], {
      mu <- mu/finS
      SS <- SS/finS
      total <- (SS-mu^2)/mu^2
      Finalsize <- finS
      return(c(  B0 = B0
                 , B1 = B1
                 , gamma = gmma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , t0 = t0
                 , RiAvg = RiAvg
                 , Finalsize=Finalsize
                 , muRi=mu
                 , totalVRi = total*mu^2
                 , totalKRi=total
      ))
    })
  })
}
Ravg<-function(sdat, B0=1, B1=0, kpa=0, omega=0, tlag=0){
  t0<-min(sdat$time)
  sfun<-approxfun(x=sdat$time, y=sdat$x, rule=2)
Ri0<-B0*exp(B1*sin(omega*tlag))*sdat$x[sdat$time==t0]^(kpa + 1)
  RiTimeAvg<-ode(times=sdat$time, func=odeR0, y=c(Ri=Ri0), parms=list(sfun=sfun,
                                                              kpa = kpa,
                                                              omega = omega,
                                                              B0 = B0,
                                                              B1 = B1,
                                                              tlag = tlag))
  return(RiTimeAvg[nrow(RiTimeAvg),"Ri"])
}
odeR0<-function(time, vals, parms){
  with(as.list(c(vals,parms)),
       {Bt<-B0*exp(B1*sin(omega*(time + tlag)))
        xt<-sfun(time)
        Rdot<-Bt*xt^(kpa + 1) 
        list(Rdot)})
}

