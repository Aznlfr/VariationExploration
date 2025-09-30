boxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    y <- sum(yvec)
    ydots <- numeric(cars)
    Bt<- B0*exp(B1*sin(omega*time))
    # gmma <=cars
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
sim <- function(kpa = 0,x0=NULL, y0=0.001, r0 = 0, B0=1,  cars = 1, finTime=365,
                timeStep=0.1, dfun=boxcar, B1=0, omega=0, gmma=0){
  if(is.null(x0)){x0 <- 1-y0}
  init_infectious <- c(y0, rep(1e-12, cars - 1))
  names(init_infectious) <- paste0("y", 1:cars)
  
  y_init <- c(x = x0, init_infectious, r=r0, cum = 0)
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=seq(from=0, to=finTime, by=timeStep)
    , parms=list(omega=omega, B0=B0, B1 = B1,  cars = cars, kpa = kpa, gmma=gmma)
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
  Bt<-parms$B0*exp(parms$B1*sin(parms$omega*time))
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