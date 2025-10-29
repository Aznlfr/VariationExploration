library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(patchwork)
library(tidyr)
library(purrr)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=10)


res_mat <- res_mat |> mutate(l_b_Uniform = remainingSus*B0
                             , u_b_Uniform = B0*(1-y0)
                             , UniformMu = (u_b_Uniform + l_b_Uniform)/2
                             , UniformVar = (u_b_Uniform - l_b_Uniform)^2/12
                             , UniformThirdRaw = (u_b_Uniform^2
                                                   + l_b_Uniform^2
                                                 )*(u_b_Uniform + l_b_Uniform)/4
                            , UniformFourthRaw = (l_b_Uniform^2*u_b_Uniform^2
                                                 +  l_b_Uniform*u_b_Uniform^3
                                                 + u_b_Uniform*l_b_Uniform^3
                                                 + u_b_Uniform^4
                                                 + l_b_Uniform^4)/5
                            , B0 = as.factor(B0)
                            )

########## Plotting Mean ################

rename_map <- c(
  "Ri_mu" = "muRi",        "uni_mu"="UniformMu",
  "Ri_var" = "totalVRi",  "uni_var"="UniformVar",
  "Ri_third"="thirdRawRi", "uni_third"="UniformThirdRaw",
  "Ri_fourth" = "fourthRawRi", "uni_fourth"="UniformFourthRaw"
)

res_mat<-res_mat|> rename(any_of(rename_map))

res_long <- res_mat |>
  pivot_longer(
    cols = matches("^(Ri|uni)_(mu|var|third|fourth)$"),
    names_to = c("source","stats"),
    names_sep = "_",
    values_to = "value"
  )

stats <- c("mu","var","third","fourth")

plots<- setNames(
  map(stats, ~ (ggplot(filter(res_long, stats == .x))
                +aes(x = B0, y=value, fill = source)
                +geom_bar(stat="identity", position=position_dodge())
                +ylab(.x)
                +xlab(bquote(beta[0]))
                +theme(axis.title.y=element_text(size = 10)))
      ), stats)




print(plots$mu/plots$var/plots$third/plots$fourth
      + plot_annotation(tag_levels ="a") 
)
