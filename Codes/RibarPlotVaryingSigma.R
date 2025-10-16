library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=5, height=5)

res_mat$sigma<-as.factor(res_mat$sigma)
barPlotRiTimeTotal<-ggplot(res_mat, aes(x = B0, y = totalVtimeRi, fill = sigma)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance in Ri \n (time-averaged)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

barPlotRiTotal<-ggplot(res_mat, aes(x = B0, y = totalVRi, fill = sigma)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance \n in Ri (weighted by incidence)") +
  xlab(bquote(beta[0])) +
  theme_classic() + theme(axis.title.y = element_text(size = 10))

Ri_var_plot_inc<- barPlotRiTotal
Ri_var_plot_time <-barPlotRiTimeTotal
print(Ri_var_plot_inc / Ri_var_plot_time + 
	plot_annotation(tag_levels ="a") 
)
