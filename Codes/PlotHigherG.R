library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
source("RcStat.R")
library(tidyr)
omega <- c(pi/30) 
B0 <- c(1.5,2,3,4,5)
B1<- c(1)
steps<- 2e3
cars <- c(1) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
gmma <- 1# waning immunity rate
t0<-50
y0<-1e-9
grid <- expand.grid(omg = omega, B0=B0)
res_mat <- as.data.frame(t(mapply(outbreakStats, omega= grid$omg, B0=grid$B0,
                                  MoreArgs = list(B1 = B1,  gmma=gmma,
                                                  cars=cars, kpa=kpas, steps=steps,
                                                  y0=y0, t0 = t0, finTime = 365
                                  ))))
res_mat_long <- pivot_longer(res_mat, cols = c(between, within),
                        names_to = "source", values_to = "RcVariance")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   



res_mat_long$omega<-round(res_mat_long$omega, digits=3)
res_mat_long$omega<-as.factor(res_mat_long$omega)
stackbar<-ggplot(res_mat_long, aes(x = B0, y = RcVariance,  fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") + plot_annotation(title="initialized at steady state")+
facet_wrap( ~ omega)
stackbar
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
ggsave("plots/barPlotRc_2.pdf", plot = stackbar, width = 7, height = 5,
      units = "in")
########Ri#######


res_mat$omega<-round(res_mat$omega, digits=3)
res_mat$omega<-as.factor(res_mat$omega)
barPlotRi<-ggplot(res_mat, aes(x = B0, y = totalVRi, fill = omega)) +
  geom_bar(stat="identity",  position=position_dodge()) +
 plot_annotation(title="initialized at steady state") + theme_classic()


ggsave("plots/barPlotRi_2.pdf", plot = barPlotRi, width = 7, height = 5,
       units = "in")

res_mat$omega<-round(res_mat$omega, digits=3)
res_mat$omega<-as.factor(res_mat$omega)
barPlotRi<-ggplot(res_mat, aes(x = B0, y = totalVRi, fill = omega)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  plot_annotation(title="initialized at steady state") + theme_classic()


ggsave("plots/barPlotRi_2.pdf", plot = barPlotRi, width = 7, height = 5,
       units = "in")
