library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
source("RcStat.R")
source("RiStat.R")
source("CommonFs.R")
library(tidyr)
omega <- c(pi/30) 
B0 <- c(1.5,2,4)
B1<- c(1)
steps<- 2e3
cars <- c(1) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
gmma <- 1# waning immunity rate
grid <- expand.grid(omg = omega, B0=B0)
res_mat <- as.data.frame(t(mapply(outbreakStats, omega= grid$omg, B0=grid$B0,
                                  MoreArgs = list(B1 = B1,  gmma=gmma,
                                                  cars=cars, kpa=kpas, steps=steps,
                                                  yint=NULL
                                  ))))
res_mat_long <- pivot_longer(res_mat, cols = c(between, within),
                        names_to = "source", values_to = "RcVariance")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   
stackbar <-ggplot(res_mat_long, aes(x = B0, y = RcVariance, fill = source)) +
  geom_bar(stat = "identity") + theme_classic() 
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)
ggsave("plots/barPlotRc.pdf", plot = stackbar, width = 7, height = 5,
       units = "in")

########Ri#######
omega <- c(0,pi/30,pi/10) 
B0 <- c(1.5,2,3,4,5)
grid <- expand.grid(omg = omega, B0=B0)
res_mat_Ri <- as.data.frame(t(mapply(outbreakStatsRi, omega= grid$omg, B0=grid$B0,
                                  MoreArgs = list(B1 = B1,  gmma=gmma,
                                                  cars=cars, kpa=kpas, steps=steps,
                                                  yint=NULL
                                  ))))
res_mat_Ri$omega<-round(res_mat_Ri$omega, digits=3)
res_mat_Ri$omega<-as.factor(res_mat_Ri$omega)
barPlotRi<-ggplot(res_mat_Ri, aes(x = B0, y = totalVRi, fill = omega)) +
  geom_bar(stat="identity",  position=position_dodge()) + theme_classic()
 

ggsave("plots/barPlotRi.pdf", plot = barPlotRi, width = 7, height = 5,
       units = "in")
