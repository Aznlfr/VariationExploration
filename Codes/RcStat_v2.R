#Another formulation to calculates Rc stats (using Exposition (2) in main.pdf)
library(deSolve)
library(shellpipes)
rpcall("Codes/RcStat_v2.Rout Codes/RcStat_v2.R")

loadEnvironments()
#library(Distributacalcul)
mderivs_v2 <- function(time, vars, parms){
  with(as.list(c(vars,parms)),{
  Bt<- plist$B0*exp(plist$alpha*sin(plist$omega*time))
  Sp<-flist$sfun(time) #fraction of susceptible
  Ri <- Bt*Sp^(plist$kpa +1)
  survival <- exp(-(time-plist$T0)) #works for cars 1
                 #WATCH OUT! # pErlang(time-T0,shape=cars, rate=cars, lower.tail=FALSE))
  return( list(c(
      survival
    , Ri*survival
    , Ri*Ri*survival
  )))
  }
  )
}
cMoments_v2 <- function(time, sfun, T0, cars, omega, alpha, kpa, B0){
  mom <- as.data.frame(ode(
    y=c(cumSurvival=0, Rc=0, RcSS=0)
    , func=mderivs_v2
    , times=time
    , parms=list(
      plist=list(T0=T0, cars=cars, omega=omega, alpha=alpha, kpa = kpa, B0=B0)
      , flist=list(sfun=sfun)
    )
  ))
  return(mom)
}
cCalc_v2 <- function(time, cohort, sfun, tol=1e-4, cars, omega, B0, alpha, kpa){
  sTime <- time[time>=cohort]
  mom <- cMoments_v2(sTime, sfun, T0=cohort, cars=cars, omega=omega,
                  alpha=alpha, kpa=kpa, B0=B0)
  with(mom[nrow(mom), ], {
    stopifnot(abs(cumSurvival-1)<tol) #for an exponential case survival is exp(-(t-t0))
    return(list(
      cohort=cohort,  Rc=Rc, varRc=(RcSS-Rc^2), RcSS=RcSS
    ))
  })
}
cohortStats_v2 <- function(kpa = 0
                           , omega = 0
                           , B0 = 1
                           , alpha = 0
                           , sdat = NULL
                           , maxCohort = NULL
                           , cohortProp=0.6
                           , dfun = boxcar
                           , cars = 1
                           ){
  sfun <- approxfun(sdat$time, sdat$x, rule=2)
  cohorts <- with(sdat, time[time<=maxCohort])
  return(as.data.frame(t(
    sapply(cohorts, function(c) cCalc_v2(sdat$time, cohort=c, sfun=sfun, tol=1e-4,
                                         cars=cars,
                                         omega = omega,
                                         B0 = B0,
                                         alpha = alpha,
                                         kpa = kpa
    ))
  )))
}
outbreakStats_v2 <- function(kpa = 0
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
                             , t0 = 0){
  mySim<- sim(kpa = kpa, omega=omega, B0=B0, alpha=alpha, timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars, sigma=sigma, y0 =y0, t0=t0
  )
    maxCohort <- t0 + cohortProp*finTime
    ifun <- approxfun(mySim$time, mySim$y*mySim$x^(kpa + 1), rule=2)
    cStats <- cohortStats_v2(kpa = kpa, omega = omega, B0 = B0, alpha = alpha,
                             sdat=mySim,
                             maxCohort=maxCohort, 
                             cars=cars)
    rcfun <- approxfun(cStats$cohort, cStats$Rc, rule=2)
    varrcfun <- approxfun(cStats$cohort, cStats$varRc, rule=2)
    rcSSfun <- approxfun(cStats$cohort, cStats$RcSS, rule=2)
    mom <- as.data.frame(ode(
      y=c(finS=0, mu=0, V=0, w=0, tv=0)
      , func=oderivs_v2
      , times=unlist(cStats$cohort)
      , parms=list(B0 = B0, alpha=alpha, omega=omega,
                    ifun=ifun, rcfun=rcfun, varrcfun=varrcfun,
                   rcSSfun = rcSSfun
      ))
    )
    with(mom[nrow(mom), ], {
      mu <- mu/finS
      within <- (V/finS)
      between <- w/finS - mu^2
      totalv2<- tv/finS - mu^2
      return(c(  stepSize=steps
                 , B0 = B0
                 , alpha = alpha
                 , sigma = sigma
                 , omega = omega
                 , cars = cars
                 , kpa = kpa
                 , muRc=mu
                 , within=within
                 , between = between
                 , total = within + between
                 , totalv2 = totalv2
      ))
    })
}
oderivs_v2 <- function(time, vars, parms){
  Bt<-parms$B0*exp(parms$alpha*sin(parms$omega*(time)))
  inc <- Bt*parms$ifun(time)
  Rc <- parms$rcfun(time)
  varRc <- parms$varrcfun(time)
  RcSS <- parms$rcSSfun(time)
  return(list(c(  
    inc #finS
    ,inc*Rc #mu
    ,inc*varRc #V
    ,inc*Rc*Rc #w
    , inc*RcSS #tv
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
