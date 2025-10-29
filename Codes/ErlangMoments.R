library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(patchwork)
library(tidyr)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=5)

res_mat <- res_mat |> mutate(carLabels = paste0("m==", cars)
                 , B0 = as.factor(B0)
                 , cars= as.factor(cars))

########## Plotting Mean ################
muRc<-(ggplot(res_mat)
      + aes(x = B0, y = muRc)
      +  geom_bar(stat = "identity")
      +  facet_wrap( ~ carLabels, labeller = label_parsed)
      +  ylab(bquote(mu))
      +  xlab(bquote(beta[0]))
      + theme(axis.title.y = element_text(size = 10))
)
############ Plotting Variance ###########
totalVRc<-(ggplot(res_mat)
           + aes(x = B0, y = totalVRc)
          +  geom_bar(stat = "identity")
          +  facet_wrap(~carLabels, labeller = label_parsed)
          +  ylab("Variance in Rc")
          +  xlab(bquote(beta[0]))
          + theme(axis.title.y = element_text(size = 10))
)
############ plotting thridRawRc ###############
thirdRawRc<-(ggplot(res_mat)
              + aes(x = B0, y = thirdRawRc)
              +  geom_bar(stat = "identity")
              + facet_wrap(~carLabels, labeller = label_parsed)
              +  ylab("Rc's third raw moment")
              +  xlab(bquote(beta[0]))
              + theme(axis.title.y = element_text(size = 10))
)
######### plotting fourthRawRc ########### fouthRawRc
fourthRawRc<-(ggplot(res_mat)
              + aes(x = B0, y = fouthRawRc)
              +  geom_bar(stat = "identity")
              +  facet_wrap( ~ carLabels, labeller = label_parsed)
              + ylab("Rc's fourth raw moment")
              +  xlab(bquote(beta[0]))
              + theme(axis.title.y = element_text(size = 10))
)

mu_and_var<-muRc + totalVRc
third_and_fourth <-thirdRawRc + fourthRawRc
print(mu_and_var / third_and_fourth + 
	plot_annotation(tag_levels ="a") 
)
