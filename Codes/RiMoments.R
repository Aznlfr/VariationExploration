library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=10)


res_mat$B0 <- as.factor(res_mat$B0)   
res_mat$cars <- factor(res_mat$cars, 
                             labels = c(paste0("m:", cars[1]),
                                        paste0("m:", cars[2]),
                                        paste0("m:", cars[3])
                                        ))
########## Plotting Mean ################
muRi<-ggplot(res_mat, aes(x = B0, y = muRi))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab(bquote(mu)) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
############ Plotting Variance ###########
totalVRi<-ggplot(res_mat, aes(x = B0, y = totalVRi))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Variance in Ri\n(weighted by incidence)") +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
########## Plotting Mean Time-averaged ################
muRiTime<-ggplot(res_mat, aes(x = B0, y = muRiTime))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab(bquote(mu ~"(Time-average)")) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
############ Plotting Variance Time-averaged ###########
totaltimeVRi<-ggplot(res_mat, aes(x = B0, y = totalVtimeRi))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Variance in Ri\n(time-averaged)") +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))


print(muRi / totalVRi / muRiTime / totaltimeVRi+ 
	plot_annotation(tag_levels ="a") 
)
