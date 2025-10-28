library(deSolve)
library(shellpipes)
rpcall("Codes/RaStat.Rout Codes/RaStat.R")
mRaderivs <- function(time, vars, parms){
  dens <- with(parms,
               cars^cars*(time - T0)^(cars-1)*exp(-cars*(time -
                                                           T0))/factorial(cars-1))
  Ra <- parms$Rasub*(time - parms$T0)
  return(with(c(parms, vars), list(c(
      dens,
    Ra*dens,
    Ra*Ra*dens,
    Ra*Ra*Ra*dens
  ))))
}

cRaMoments <- function(time, Rasub, T0, cars){
  mom <- as.data.frame(ode(
    y=c(cumden=0, Ratot=0, RaSS=0, RaS3 = 0)
    , func=mRaderivs
    , times=time
    , parms=list(
      T0=T0, cars=cars, Rasub = Rasub)
  ))
  return(mom)
}


RaCCalc <- function(sdat, cohort, ST, tol=1e-4, cars, omega, B0, alpha, kpa){
  with(sdat, { 
    ST0<- ST^(kpa + 1)
    BT0 <- B0*exp(alpha*sin(omega*cohort))
    sTime <- time[time>=cohort]
    mom <- cRaMoments(sTime, Rasub = BT0*ST0, T0=cohort, cars = cars)
    with(mom[nrow(mom), ], {
      stopifnot(abs(cumden-1)<tol)
      Ratot=Ratot/cumden
      RaSS=RaSS/cumden
      RaS3 = RaS3/cumden
      return(list(
        cohort=cohort, Ra=Ratot, varRa=(RaSS-Ratot^2), RaSS =RaSS, RaS3=RaS3
      ))
    })
  })
}
RaCohortStats <- function(kpa = 0
                          , omega = 0
                          , B0 = 1
                          , alpha = 0
                          , sdat = NULL
                          , maxCohort = NULL
                          , cohortProp=0.6
                          , dfun = boxcar
                          , cars = 1
                          , ...){
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) RaCCalc(sdat = sdat, cohort=c, ST=sfun(c),
                                        tol=1e-4,
                                        cars=cars,
                                        omega = omega,
                                        B0 = B0,
                                        alpha = alpha,
                                        kpa = kpa
    ))
  )))
}


outbreakStatsRa <- function(kpa = 0
                            , omega=0
                            , B0=1
                            , alpha = 0
                            #  , tmult=6
                            , cohortProp=0.6
                            , steps=300
                            , dfun = boxcar
                            , cars = 1
                            , sigma = 0
                            , finTime = 365
                            , y0 = 1e-9
                            , t0 = 0
){
  mySim<- sim(kpa = kpa, omega=omega, B0=B0, alpha=alpha, timeStep=finTime/steps,
              y0 = y0, finTime=finTime, dfun=dfun, cars=cars, sigma=sigma
  )
  with(mySim, {
    ifun <- approxfun(time, y*x^(kpa + 1), rule=2)
    cStats <- RaCohortStats(kpa = kpa, omega = omega, B0 = B0, alpha=alpha,
                            sdat=mySim,
                            maxCohort=cohortProp*finTime, 
                            cars=cars)
    rafun <- approxfun(cStats$cohort, cStats$Ra, rule=2)
    varrafun <- approxfun(cStats$cohort, cStats$varRa, rule=2)
    wssfun <- approxfun(cStats$cohort, cStats$RaSS, rule = 2)
    ws3fun <- approxfun(cStats$cohort, cStats$RaS3, rule=2)
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, SS=0, thirdRaw = 0, V=0, w = 0, checkV = 0)
      , func=oderivsRa
      , times=unlist(cStats$cohort)
      , parms=list( B0 = B0, alpha=alpha, omega=omega,
                    ifun=ifun, rafun=rafun, varrafun=varrafun,
                    wssfun = wssfun, ws3fun = ws3fun))
    )
    
    with(mom[nrow(mom), ], {
      mu <- mu/finS
      SS <- SS/finS
      w <- w/finS
      thirdRaw <- thirdRaw/finS
      checkV <- (checkV/finS)/mu^2
      within <- (V/finS)/mu^2
      between <- (SS-mu^2)/mu^2
      total = within + between
      otherCheck = (w-mu^2)/mu^2
      return(c(    B0 = B0
                 , alpha = alpha
                 , sigma = sigma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , remainingSus = 1 - y0 - finS
                 , muRa=mu
                 , withinRa=within
                 , checkWithinRa = checkV
                 , betweenRa=between
                 , simplifiedTotalVRa = w - mu^2
                 , totalVRa = total*mu^2
                 , SecRawRa = w
                 , thirdRawRa = thirdRaw
                 
      ))
    })
  })
}
oderivsRa <- function(time, vars, parms){
  Bt<-parms$B0*exp(parms$alpha*sin(parms$omega*(time)))
  inc <- Bt*parms$ifun(time)
  Ra <- parms$rafun(time)
  varRa <- parms$varrafun(time)
  wss <- parms$wssfun(time)
  ws3<-parms$ws3fun(time)
  return(list(c(  
    inc #finS
    ,inc*Ra #mu
    ,inc*Ra*Ra #RSS
    ,inc*ws3
    ,inc*varRa #V
    ,inc*wss #w
    ,inc*(wss - Ra^2) #checkV
  )))
}
boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0*exp(alpha*sin(omega*time))
    xdot <- -Bt*y*x^(kpa+1) + cars*sigma*r
    ydots[[1]] <- Bt*y*x^(kpa+1) - cars*yvec[[1]]
    cumdot <- Bt*y*x^(kpa+1)
    if (cars > 1) {
      ydots[2:cars] <- cars * (yvec[1:(cars - 1)] - yvec[2:cars])
    }
    rdot <- cars*yvec[[cars]] - cars*sigma*r
    out <- c(xdot, ydots, rdot, cumdot)
    names(out) <- c("xdot", paste0("y", 1:cars, "dot"), "rdot", "cumdot")
    return(list(out))
  }
  )
}
sim <- function(kpa = 0, B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar, alpha=0, omega=0, sigma=0, t0 =0, 
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
    , parms=list(omega=omega, B0=B0, alpha = alpha,  cars = cars,
                 kpa = kpa, sigma=sigma)
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

saveEnvironment()
