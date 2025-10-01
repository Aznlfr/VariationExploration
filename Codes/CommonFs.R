boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0*exp(B1*sin(omega*(time + t0)))
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
                timeStep=0.1, dfun=boxcar, B1=0, omega=0, gmma=0, yvec0 =NULL){
  if(is.null(yvec0)){
    y0 <- 1e-9
    x0 <- 1-y0
    r0 <- 0
    infc <- c(y0, rep(1e-12, cars - 1))
    cum0<-0
    t0<-0
  }else{
    infc <- unlist(yvec0[grep("^y[0-9]$", names(yvec0))])
    x0<- yvec0$x
    r0<- yvec0$r
    cum0<-yvec0$cum
    t0<-yvec0$time
  }
  names(infc) <- paste0("y", 1:cars)
  y_init <- c(x = x0, infc, r=r0, cum = cum0)
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=seq(from=0, to=finTime, by=timeStep)
    , parms=list(omega=omega, B0=B0, B1 = B1,  cars = cars,
                 kpa = kpa, gmma=gmma, t0=t0)
  ))
  
  return(within(sim, {
    if (cars>1){
      y <- rowSums(as.data.frame(mget(paste0("y", 1:cars))))
    }
    else{
      y <- y1
    }
    z <- 1-x-y
  }))
}

oderivs <- function(time, vars, parms){
  Bt<-parms$B0*exp(parms$B1*sin(parms$omega*(time + parms$t0)))
  inc <- Bt*parms$ifun(time)
  Rc <- parms$rcfun(time)
  varRc <- parms$varrcfun(time)
  wss <- parms$wssfun(time)
  return(list(c(
    finSdot = inc
    , mudot = inc*Rc
    , SSdot = inc*Rc*Rc
    , Vdot = inc*varRc
    , wdot = inc*wss
    , checkVdot = inc*(wss - Rc^2)
    
  )))
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
                              , yint = NULL
                              , maxCohort = NULL
){
  if(is.null(yint)){t0<-0}
  else{t0<-yint$time}
  sdat<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars, gmma=gmma, yvec0 = yint
  )
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
cohortStatsRiPlot <- function(kpa = 0
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
                              , maxCohort = NULL
){
  if(is.null(yint)){t0<-0}
  else{t0<-yint$time}
  sdat<- sim(kpa = kpa, omega=omega, B0=B0, B1=B1, timeStep=finTime/steps,
             finTime=finTime, dfun=dfun, cars=cars, gmma=gmma, yvec0 = yint
  )
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
simwrap <- function(kpa = 0, B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar, B1=0, omega=0, gmma=0, yvec0 =NULL){
  sdat<-sim(kpa = kpa, B0 =B0, cars=cars, finTime=finTime, timeStep = timeStep,
           dfun=dfun, B1=B1, omega=omega, gmma = gmma, yvec0=yvec0)
  return(
    list(
      sdat = sdat, finTime = finTime
    )
  )
}