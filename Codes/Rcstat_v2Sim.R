
library(deSolve)
library(shellpipes)

loadEnvironments()


year <- 60
tau <- 2*pi
#omega <- c(tau/year)
omega <- 0
B0 <- c(1.5,3.5,5,8)
alpha<- c(0)
sigma <- 0
steps<- 5e3
kpas <- c(0)  #the variation in the susceptibility
t0<-0
finTime<-150
y0<-1e-9


res_mat<-as.data.frame(t(sapply(
  B0, function(x) outbreakStats_v2( B0=x,
  alpha=alpha,
                  omega=omega,
                  cars=1,
                  kpa=kpas,
                  finTime=finTime,
                  steps=steps,
                  sigma=sigma,
                  y0=y0,
                  t0=t0))))

saveEnvironment()