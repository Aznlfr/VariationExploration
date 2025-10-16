library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=5)


res_mat$B0 <- as.factor(res_mat$B0)   
res_mat$cars <- factor(res_mat$cars, 
                             labels = c(paste0("m:", cars[1]),
                                        paste0("m:", cars[2]),
                                        paste0("m:", cars[3])
                                        ))
########## Plotting Mean ################
muRc<-ggplot(res_mat, aes(x = B0, y = muRc))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab(bquote(mu)) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
############ Plotting Variance ###########
totalVRc<-ggplot(res_mat, aes(x = B0, y = totalVRc))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Variance in Rc") +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
############ plotting thridRawRc ###############
thirdRawRc<-ggplot(res_mat, aes(x = B0, y = thirdRawRc))  +
  geom_bar(
    stat = "identity") +
facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Rc's third raw moment") +
 xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
######### plotting fourthRawRc ########### fouthRawRc
fourthRawRc<-ggplot(res_mat, aes(x = B0, y = fouthRawRc))  +
  geom_bar(
    stat = "identity") +
  facet_wrap( ~ cars, labeller = label_parsed) +
  ylab("Rc's fourth raw moment") +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))

mu_and_var<-muRc + totalVRc
third_and_fourth <-thirdRawRc + fourthRawRc
print(mu_and_var / third_and_fourth + 
	plot_annotation(tag_levels ="a") 
)
