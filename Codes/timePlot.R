library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(patchwork)
library(tidyr)
library(shellpipes)

loadEnvironments()

startGraphics(width=7, height=5)

rc <- (ggplot(cohorts)
      +  aes(cohort, Rc, color = as.factor(B0))
      +  geom_hline(yintercept = 1)
      +  geom_line(linewidth = 1, alpha = 0.8)
      +  theme_classic()
      +  scale_color_viridis_d()
      +  guides(color = "none")
      +   coord_cartesian(xlim =c(t0,xlimits))
      +  labs(x = "cohort infection time",
              y = "mean cohort \ncases per case")
      )


VarRc <- (ggplot(cohorts)
          +aes(cohort, varRc, color = as.factor(B0))
          +  geom_line(linewidth = 1, alpha = 0.8)
          +  geom_hline(yintercept = 1)
          +  theme_classic()
          +  scale_color_viridis_d(name = expression(B[0]), guide="none")
          + coord_cartesian(xlim =c(t0,xlimits))
          +  labs(x = "cohort infection time",
                  y = "Variance cohort \ncases per case" )
)

SusPlot  <- (ggplot(straightSim)
            + aes(time, x, color = as.factor(B0))
            + geom_line(linewidth = 1)
            + scale_color_viridis_d(guide="none")
            +  labs(x = "time", y = "susptible (x(t))", color = "B0")
            +  coord_cartesian(xlim =c(t0,xlimits))
            +  theme_classic()
            )

IPlot  <- (ggplot(straightSim)
          + aes(time, y, color = as.factor(B0))
          + geom_line(linewidth = 1)
          + scale_color_viridis_d(guide="none")
          +  labs(x = "time", y = "infectious (y(t))", color = "B0")
          +  coord_cartesian(xlim =c(t0,xlimits))
          +  theme_classic()
)

Beta  <- straightSim |>
            mutate(Bt = B0*exp(alpha*sin(omega*time))) |>
            ggplot(aes(time, Bt, color = as.factor(B0))) +
            geom_line(linewidth = 1)+ 
            scale_color_viridis_d(guide="none")+
            labs(x = "time", y = bquote(beta(t)), color = "B0") +
            coord_cartesian(xlim =c(t0,xlimits)) +
            theme_classic() 

incidence  <- (ggplot(straightSim)
               +  aes(time, inc, color = as.factor(B0))
               +  geom_line(linewidth = 1)
               +   scale_color_viridis_d()
               +  labs(x = "time", y = "incidence"
                       , color = "B0")
               +  coord_cartesian(xlim =c(t0,xlimits))
               +  theme_classic()
               +  theme(legend.position = "inside"
                        , legend.position.inside = c(0.9, 0.9))
)

ri  <- straightSim |>
  mutate(Ri = B0*exp(sin(alpha*omega*time))*x) |>
  ggplot(aes(time, Ri, color = as.factor(B0))) +
  geom_line(linewidth = 1)+ 
  scale_color_viridis_d(guide="none")+
  labs(x = "time", y = "Ri(t)", color = "B0") +
  coord_cartesian(xlim =c(t0,xlimits)) +
   theme_classic() 

finalp<- incidence + IPlot + SusPlot + VarRc+  rc +  ri + Beta+
  plot_annotation(tag_levels ="a") +
  plot_annotation(
    title = bquote("T:" ~ .(2*pi/omega) ~ ","~alpha ~ ":" ~ .(alpha) ~ 
                     ", " ~ sigma ~ ":" ~ .(sigma) ~ ", " ~ kappa ~ ":" ~ .(kpa))
  )

print(finalp)


