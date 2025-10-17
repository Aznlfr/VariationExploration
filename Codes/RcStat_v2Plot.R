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
                             names_to = "source", values_to = "RcVarianceV2")
res_mat_long$B0 <- as.factor(res_mat_long$B0)   

stackbar<-ggplot(res_mat_long, aes(x = B0, y = RcVarianceV2,  fill = source))  +
  geom_bar(
    stat = "identity",
    position = "stack") +
  ylab("Variance in Rc (v2)") +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))



barPlotRcTotal<-ggplot(res_mat, aes(x = B0, y = totalv2)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  ylab("total Variance \n in Rc (total v2)") +
  xlab(bquote(beta[0])) +
  theme(axis.title.y = element_text(size = 10))

print(stackbar / barPlotRcTotal + 
        plot_annotation(tag_levels ="a") 
)
