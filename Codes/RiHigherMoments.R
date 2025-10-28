library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(tidyr)
library(deSolve)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=10)

B0vec<-unique(res_mat$B0)
remainingSusptible = res_mat$remainingSus
l_b_Uniform<-remainingSusptible*B0vec
u_b_Uniform<-B0vec*(1-y0)
UniformMu<-(u_b_Uniform + l_b_Uniform)/2
UniformVar <- (u_b_Uniform - l_b_Uniform)^2/12
UniformThirdRaw <- (u_b_Uniform^2 + l_b_Uniform^2)*(u_b_Uniform + l_b_Uniform)/4
UniformFourthRaw <- (l_b_Uniform^2*u_b_Uniform^2 +
                      l_b_Uniform*u_b_Uniform^3 + u_b_Uniform*l_b_Uniform^3 +
                       u_b_Uniform^4 + l_b_Uniform^4)/5
UniformStat<-data.frame(B0 = B0vec, mu = UniformMu,
                        var = UniformVar,
                        thirdRaw = UniformThirdRaw,
                        fourthRaw = UniformFourthRaw)
res_mat$UniformMu <- UniformStat[match(res_mat$B0, UniformStat$B0),"mu"]
res_mat$UniformVar <- UniformStat[match(res_mat$B0, UniformStat$B0),"var"]
res_mat$Uniform3Raw <- UniformStat[match(res_mat$B0,UniformStat$B0), "thirdRaw"]
res_mat$Uniform4Raw <- UniformStat[match(res_mat$B0,UniformStat$B0), "fourthRaw"]
res_mat$B0 <- as.factor(res_mat$B0)   

########## Plotting Mean ################
res_mat_mu <- pivot_longer(res_mat,
                        cols = c(muRi, UniformMu),
                        names_to = "fill",
                        values_to = "mu") 

first<- ggplot(res_mat_mu, aes(x = B0, y = mu, fill =fill )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab(bquote(mu)) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))

res_mat_var <- pivot_longer(res_mat,
                           cols = c(totalVRi, UniformVar),
                           names_to = "fill",
                           values_to = "var")

second<-ggplot(res_mat_var, aes(x = B0, y = var, fill = fill)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab(bquote(sigma^2)) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))


res_mat_3rd <- pivot_longer(res_mat,
                            cols = c(thirdRawRi, Uniform3Raw),
                            names_to = "fill",
                            values_to = "third")

third<-ggplot(res_mat_3rd, aes(x = B0, y = third, fill = fill)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab(bquote(E(R[i]^3))) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))

res_mat_4rd <- pivot_longer(res_mat,
                            cols = c(fourthRawRi, Uniform4Raw),
                            names_to = "fill",
                            values_to = "fourth")

fourth<-ggplot(res_mat_4rd, aes(x = B0, y = fourth, fill = fill)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab(bquote(E(R[i]^4))) +
  xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))



print(first / second / third/ fourth + 
        plot_annotation(tag_levels ="a") 
)
saveEnvironment()