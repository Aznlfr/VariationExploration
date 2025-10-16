library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=7, height=5)

rc <- cohorts |> ggplot(aes(cohort, Rc, color = as.factor(B0))) +
  geom_hline(yintercept = 1) +
  geom_line(linewidth = 1, alpha = 1) +
  theme_classic() +
  scale_color_viridis_d(name = "B_0") +
  guides(color = "none") +
  xlim(c(t0, xlimits)) +
  labs(x = "cohort infection time", y = "mean cohort \ncases per case")


VarRc <- cohorts |>
  ggplot(aes(cohort, varRc, color = as.factor(B0))) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 1) +
  theme_classic() +
  scale_color_viridis_d(name = "B_0") +
  guides(color="none") +
  xlim(c(t0, xlimits)) +
  labs(x = "cohort infection time", y = "Variance cohort \ncases per case" )

SusPlot  <- straightSim |>
  ggplot(aes(time, x, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "susptible (x(t))", color = "B0") +
  xlim(c(t0,xlimits))+
  guides(color="none") +
  theme_classic()

IPlot  <- straightSim |>
  ggplot(aes(time, y, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "infectious (y(t))", color = "B0") +
  xlim(c(t0,xlimits))+
  theme_classic() +
  guides(color="none")

Beta  <- straightSim |>
  mutate(Bt = B0*exp(alpha*sin(omega*time))) |>
  ggplot(aes(time, Bt, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = bquote(beta(t)), color = "B0") +
  xlim(c(t0,xlimits))+
  theme_classic() +
  guides(color="none")

incidence  <- straightSim |>
  ggplot(aes(time, inc, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "incidence", color = "B0") +
  xlim(c(t0,xlimits))+
  theme_classic() +
  theme(legend.position = "inside"
        , legend.position.inside = c(0.9, 0.9))

ri  <- straightSim |>
  mutate(Ri = B0*exp(sin(alpha*omega*time))*x) |>
  ggplot(aes(time, Ri, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d()+
  labs(x = "time", y = "Ri(t)", color = "B0") +
  xlim(c(t0,xlimits))+
  theme_classic() +
  guides(color="none")


finalp<- incidence + IPlot + SusPlot + VarRc+  rc +  ri + Beta+
  guides(color = "none") +
  xlim(c(t0, xlimits)) + 
  plot_annotation(tag_levels ="a") +
  plot_annotation(
    title = bquote("T:" ~ .(2*pi/omega) ~ ","~alpha ~ ":" ~ .(alpha) ~ 
                     ", " ~ sigma ~ ":" ~ .(sigma) ~ ", " ~ kappa ~ ":" ~ .(kpa))
  )

print(finalp)


