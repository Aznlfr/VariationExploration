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
UniformMu<-B0vec/2
UniformVar <- B0vec^2/12
UniformThirdRaw <- B0vec^3/4
UniformFourthRaw <- B0vec^4/5
UniformStat<-data.frame(B0 = B0vec, mu = UniformMu, var = UniformVar,
                        thirdRaw = UniformThirdRaw, fourthRaw = UniformFourthRaw)
res_mat$UniformMu <- UniformStat[match(res_mat$B0, B0),"mu"]
res_mat$UniformVar <- UniformStat[match(res_mat$B0, B0),"var"]
res_mat$Uniform3Raw <- UniformStat[match(res_mat$B0,B0), "thirdRaw"]
res_mat$Uniform4Raw <- UniformStat[match(res_mat$B0,B0), "fourthRaw"]
res_mat$B0 <- as.factor(res_mat$B0)   

########## Plotting Mean ################
res_mat_mu <- pivot_longer(res_mat,
                        cols = c(muRi, UniformMu),
                        names_to = "fill",
                        values_to = "mu") 

first<- ggplot(res_mat_mu, aes(x = B0, y = mu, fill = fill)) +
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

# fourth
# 
# muRi<-ggplot(res_mat, aes(x = B0, y = muRi))  +
#   geom_bar(
#     stat = "identity") +
#   ylab(bquote(mu)) +
#   xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
# ############ Plotting Variance ###########
# totalVRi<-ggplot(res_mat, aes(x = B0, y = totalVRi))  +
#   geom_bar(
#     stat = "identity") +
#   ylab("Variance in Ri\n(weighted by incidence)") +
#   xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
# ############ Plotting normalized third Raw moments ###########
# thirdRi<-ggplot(res_mat, aes(x = B0, y = thirdRawRi))  +
#   geom_bar(
#     stat = "identity") +
#   ylab("3rd raw moment of Ri\n(weighted by incidence)") +
#   xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))
# ########## Plotting normalized fourth Raw moments################
# fourthRi<-ggplot(res_mat, aes(x = B0, y = fourthRawRi))  +
#   geom_bar(
#     stat = "identity") +
#   ylab("fourth raw moment of Ri\n(weighted by incidence)") +
#   xlab(bquote(beta[0])) + theme(axis.title.y = element_text(size = 10))


print(first / second / third/ fourth + 
        plot_annotation(tag_levels ="a") 
)
