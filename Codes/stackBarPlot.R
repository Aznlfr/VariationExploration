library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)
rpcall("Codes/stackBarPlot.Rout Codes/stackBarPlot.R Codes/RcStat.Rout Codes/RcStat.rda")

loadEnvironments()

omega <- c(pi/30) 
B0 <- c(1.5,3.5,5,8)
alpha<- c(1)
steps<- 5e3
cars <- c(1,2,3) #number of compartments
kpas <- c(0)  #the variation in the susceptibility
#sigma <- c(0.02, 0.05, 0.1)# waning immunity rate
sigma<-0.1
t0<-100
finTime<-100
y0<-1e-9
grid <- expand.grid(B0=B0, cars=cars)
res_mat <- as.data.frame(t(mapply(outbreakStats, B0=grid$B0,
                                  cars=grid$cars,
                                  MoreArgs = list(alpha=alpha,
                                                  omega=omega,
                                                  sigma=sigma,
                                                  kpa=kpas,
                                                  finTime=finTime,
                                                  steps=steps,
                                                  y0=y0,
                                                  t0=t0))))
###### plotting Variance in Rc weighted by incidence #####
res_mat_long <- pivot_longer(res_mat, cols = c(between, within),
                        names_to = "source", values_to = "RcVariance")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   
res_mat_long$cars <- factor(res_mat_long$cars, 
                             labels = c(paste0("m:", cars[1]),
                                        paste0("m:", cars[2]),
                                        paste0("m:", cars[3])))

stackbar<-ggplot(res_mat_long, aes(x = B0, y = RcVariance,  fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") +
facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Variance in Rc \n(weighted by incidence)") +
 xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
####### plotting Variance in Rc (time-averaged) #####
res_mat_long <- pivot_longer(res_mat, cols = c(betweenTime, withinTime),
                             names_to = "source", values_to = "RcVarianceTime")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   
res_mat_long$cars <- factor(res_mat_long$cars, 
                             labels = c(paste0("m:", cars[1]),
                                        paste0("m:", cars[2]),
                                        paste0("m:", cars[3])))
stackbar_RcTime<-ggplot(res_mat_long, aes(x = B0, y = RcVarianceTime, 
                                          fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") +
  facet_wrap(
    ~ cars,
    labeller = label_parsed
  ) +
  ylab("Variance in Rc\n (time-averaged)") +
 xlab(bquote(beta[0]))+ theme(axis.title.y = element_text(size = 10))

res_mat$cars<-as.factor(res_mat$cars)
barPlotRcTimeTotal<-ggplot(res_mat, aes(x = B0, y = totalVtime, fill = cars)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance in Rc \n (time-averaged)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

barPlotRcTotal<-ggplot(res_mat, aes(x = B0, y = totalVRc, fill = cars)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance \n in Rc (weighted by incidence)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

Rc_var_plot_inc<-stackbar + barPlotRcTotal
Rc_var_plot_time <-stackbar_RcTime+ barPlotRcTimeTotal
Rc_var_plot <- Rc_var_plot_inc / Rc_var_plot_time + 
  plot_annotation(tag_levels ="a") 

# if (!dir.exists("Codes/plots")) dir.create("Codes/plots", recursive = TRUE)
ggsave("Codes/plots/RcbarPlotVaryingCar.pdf",
       plot = Rc_var_plot, width = 10,
       height = 5,
       units = "in")
########Ri#######
res_mat$cars<-as.factor(res_mat$cars)
Ri_var_plot_inc<-ggplot(res_mat, aes(x = B0, y = totalVRi, fill = cars)) +
  geom_bar(stat="identity",  position=position_dodge()) + 
  ylab("total Variance in \n Ri (weighted by incidence)") +
  theme_classic() +
    xlab(bquote(beta[0])) +
    theme(axis.title.y = element_text(size=10))

Ri_var_plot_time<-ggplot(res_mat, aes(x = B0, y = totalVtimeRi, fill = cars)) +
  geom_bar(stat="identity",  position=position_dodge()) +
ylab("total Variance in \n Ri (time-averaged)") +
theme_classic() +
  xlab(bquote(beta[0])) +
  theme(axis.title.y = element_text(size=10))
Ri_var_plot <- Ri_var_plot_inc + Ri_var_plot_time

 ggsave("Codes/plots/RibarPlotVaryingCars.pdf", plot = Ri_var_plot, width = 7,
        height = 5,
        units = "in")
saveEnvironment()
