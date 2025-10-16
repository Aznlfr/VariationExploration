library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=5)

res_mat_long <- pivot_longer(res_mat, cols = c(between, within),
                        names_to = "source", values_to = "RcVariance")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   
res_mat_long$sigma <- factor(res_mat_long$sigma, 
                             labels = c(paste0("sigma:", sigma[1]),
                                        paste0("sigma:", sigma[2]),
                                        paste0("sigma:", sigma[3])))

stackbar<-ggplot(res_mat_long, aes(x = B0, y = RcVariance,  fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") +
facet_wrap( ~ sigma, labeller = label_parsed) +
  ylab("Variance in Rc \n(weighted by incidence)") +
 xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
####### plotting Variance in Rc (time-averaged) #####
res_mat_long <- pivot_longer(res_mat, cols = c(betweenTime, withinTime),
                             names_to = "source", values_to = "RcVarianceTime")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   
res_mat_long$sigma <- factor(res_mat_long$sigma, 
                             labels = c(paste0("sigma:", sigma[1]),
                                        paste0("sigma:", sigma[2]),
                                        paste0("sigma:", sigma[3])))
stackbar_RcTime<-ggplot(res_mat_long, aes(x = B0, y = RcVarianceTime, 
                                          fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") +
  facet_wrap(
    ~ sigma,
    labeller = label_parsed
  ) +
  ylab("Variance in Rc\n (time-averaged)") +
 xlab(bquote(beta[0]))+ theme(axis.title.y = element_text(size = 10))

res_mat$sigma<-as.factor(res_mat$sigma)
barPlotRcTimeTotal<-ggplot(res_mat, aes(x = B0, y = totalVtime, fill = sigma)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance in Rc \n (time-averaged)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

barPlotRcTotal<-ggplot(res_mat, aes(x = B0, y = totalVRc, fill = sigma)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance \n in Rc (weighted by incidence)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

Rc_var_plot_inc<-stackbar + barPlotRcTotal
Rc_var_plot_time <-stackbar_RcTime+ barPlotRcTimeTotal
print(Rc_var_plot_inc / Rc_var_plot_time + 
	plot_annotation(tag_levels ="a") 
)
