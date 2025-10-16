library(deSolve)
library(shellpipes)

GIboxcar <- function(time, vars, parms){
  with(as.list(c(vars, parms)), {
    yvec <- (unlist(mget(paste0("y", 1:cars))))
    ydots <- numeric(cars)
    ydots[[1]] <--cars*yvec[[1]]
    if (cars > 1) {
      ydots[2:cars] <- cars * (yvec[1:(cars - 1)] - yvec[2:cars])
    }
    out <- ydots
    return(list(out))
  }
  )
}
GIsim <- function(cars = 1, finTime=365,
                  timeStep=0.1, dfun=GIboxcar 
){
  if(cars>1){
    infc <- c(1,rep(0,cars-1))}
  else{
    infc <- c(1)
  }
  names(infc) <- paste0("y", 1:cars)
  y_init <- infc
  timePoints<- seq(from=0, to=finTime, by=timeStep)
  sim <- as.data.frame(ode(
    y = y_init
    , func=dfun
    , times=timePoints
    , parms=list(cars = cars)
  ))
  return(within(sim, {
    if (cars>1){
      y <- rowSums(as.data.frame(mget(paste0("y", 1:cars))))
    }
    else{
      y <- y1
    }
    
    
  }))
}

GICV <- function(omega=0
                 , B0=1
                 , alpha = 0
                 , steps=300
                 , dfun=GIboxcar 
                 , cars = 1
                 , sigma = 0
                 , finTime = 365
                 , tol = 1e-4
                 , subdivisions = 1e4
                 
                 ){
  myGISim<- GIsim(timeStep=finTime/steps,
              finTime=finTime, dfun=dfun, cars=cars)
  with(myGISim,{
  Bt<- B0*exp(alpha*sin(omega*time))
  f_instantanous_inf <-approxfun(x=time, y=Bt*y, rule=2) 
  R0<-integrate(f_instantanous_inf, lower =0 , upper =Inf , 
                subdivisions = subdivisions)
  GIstats <- as.data.frame(ode(
    y=c(GIcdf=0, GImu=0, GISS=0)
    , func=GIdstat
    , times=time
    , parms=list(R0 = R0[["value"]],
                 GIdis = f_instantanous_inf))
  )
  with(GIstats[nrow(GIstats),],{
    stopifnot(abs(GIcdf -1)<tol)
    GImu <- GImu/GIcdf
    GIvar <- GISS/GIcdf - GImu^2
    GICV2 <- GIvar/GImu^2
    GICV2theory<- (cars + 5)/(3*(cars + 1))
    return(c(
      GImu = GImu,
      GIvar = GIvar,
      GICV2 = GICV2,
      GICV2theory = GICV2theory
  
    )
    )
  })
  })
  
}
GIdstat<-function(time, vars, parms){
  with(as.list(c(vars,parms)),{
    GIcdfdot<-GIdis(time)/R0
    GImudot<- time*GIdis(time)/R0
    GISSdot<- time*time*GIdis(time)/R0
    return(list(c(GIcdfdot, GImudot, GISSdot)))
    
  }
       )
}
tau <- 2*pi
cars <- 1
finTime <- 1000
steps <- 3e5
B0 <- 2
alpha <- 0
omega <- tau/60
subdivisions <-1e5
GIstats<-GICV(cars=cars, finTime=finTime, steps=steps, B0=B0, omega=omega,
              alpha = alpha, subdivisions = subdivisions)
print(GIstats)
saveEnvironment()
