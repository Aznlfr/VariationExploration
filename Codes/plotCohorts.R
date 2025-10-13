library(shellpipes)
rpcall("Codes/plotCohorts.Rout Codes/plotCohorts.R Codes/RcStat.Rout Codes/RcStat.rda")
library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(deSolve)
loadEnvironments()
B0 <- c(2,4)
alpha<-1
steps<-3e3
kpa<-0
omega<-pi/30
cars<-1
sigma<-c(0.1)
finTime<-365
y0 <-1e-9
t0 <-0
xlimits<-finTime
cohorts <- map_dfr(B0, function(B0){
  return(data.frame(sapply(cohortStatsRcPlot(kpa = kpa
                                             , omega=omega
                                             , B0=B0
                                             , alpha = alpha
                                             , steps=steps
                                             , cars = cars
                                             , sigma = sigma
                                             , finTime = finTime
                                             , y0 = y0
                                             , t0=t0
                                             , cohortProp = 0.6
                                             ),
                           unlist), B0 = B0))
}
)

straightSim <- map_dfr(B0, function(B0){
  return(data.frame(sim(kpa = kpa, omega=omega, B0=B0, alpha=alpha, 
                        timeStep=finTime/steps,
                        finTime=finTime,  cars=cars,
                        sigma=sigma, y0 = 1e-9, t0 = t0
  ), B0 = B0))
}
)

rc <- cohorts |> ggplot(aes(cohort, Rc, color = as.factor(B0))) +
  geom_hline(yintercept = 1) +
  geom_line(linewidth = 1, alpha = 1) +
  theme_classic() +
  scale_color_viridis_d(name = "B_0") +
  guides(color = "none") +
  xlim(c(0, xlimits)) +
  labs(x = "cohort infection time", y = "mean cohort \ncases per case")



kc <- cohorts |>
  mutate(kappa_c = varRc/Rc^2) |>
  ggplot(aes(cohort, kappa_c, color = as.factor(B0))) +
  geom_line(linewidth = 1, alpha = 0.8) +
  theme_classic() +
  scale_color_viridis_d(name = "B_0") +
  guides(color="none") +
  labs(x = "cohort infection time", y = "CV^2 for cohort cases per case" )

VarRc <- cohorts |>
  ggplot(aes(cohort, varRc, color = as.factor(B0))) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  scale_color_viridis_d(name = "B_0") +
  guides(color="none") +
  xlim(c(0, xlimits)) +
  labs(x = "cohort infection time", y = "Variance cohort \ncases per case" )

SusPlot  <- straightSim |>
  ggplot(aes(time, x, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "susptible (x(t))", color = "B0") +
  xlim(c(0,xlimits))+
  guides(color="none") +
  theme_classic()

IPlot  <- straightSim |>
  ggplot(aes(time, y, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "infectious (y(t))", color = "B0") +
  xlim(c(0,xlimits))+
  theme_classic() +
  theme(legend.position = "inside"
        , legend.position.inside = c(0.7, 0.9))

Beta  <- straightSim |>
  mutate(Bt = B0*exp(alpha*sin(omega*time))) |>
  ggplot(aes(time, Bt, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "B(t)", color = "B0") +
  xlim(c(0,xlimits))+
  theme_classic() +
  guides(color="none")

incidence  <- straightSim |>
  ggplot(aes(time, inc, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "incidence", color = "B0") +
  xlim(c(0,xlimits))+
  theme_classic() +
  guides(color="none")

ri  <- straightSim |>
  mutate(Ri = B0*exp(sin(alpha*omega*time))*x) |>
  ggplot(aes(time, Ri, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "Ri(t)=B(t)x(t)", color = "B0") +
  xlim(c(0,xlimits))+
  theme_classic() +
  guides(color="none")


 finalp<- incidence + IPlot + SusPlot + VarRc+  rc +  ri + Beta+
   guides(color = "none") +
   xlim(c(0, xlimits)) + 
  plot_annotation(tag_levels ="a") +
   plot_annotation(
     title = bquote(omega ~ ":" ~ .(round(omega, digits=3)) ~ ","~alpha ~ ":" ~ .(alpha) ~ 
                      ", " ~ sigma ~ ":" ~ .(sigma) ~ ", " ~ kappa ~ ":" ~ .(kpa))
   )

if (!dir.exists("Codes/plots")) dir.create("Codes/plots", recursive = TRUE)
ggsave("Codes/plots/dummy.pdf", plot = finalp, width = 7,
     height = 5,
    units = "in")


saveEnvironment()
