library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tidyr)
library(shellpipes)

loadEnvironments()

startGraphics(width=3, height=4)

res_mat_long <- (res_mat
	|> pivot_longer(cols = c(between, within),
		names_to = "source", values_to = "RcVariance"
	)
	|> filter(kpa != 0.5)
)

res_mat_long$B0 <- as.factor(res_mat_long$B0)   
res_mat_long$kpa <- factor(res_mat_long$kpa, 
	labels = c(paste0("kappa ==", kpas[1]),
  	paste0("kappa ==", kpas[3]))
)

stackbar <- (ggplot(res_mat_long)
	+ aes(x = B0, y = RcVariance,  fill = source)
	+ geom_bar(
		stat = "identity",
		position = "stack"
	)
	+ facet_wrap( ~ kpa, labeller = label_parsed)

	+ ylab(bquote("Variance in " ~ R[c]))
	+ xlab(bquote(R[0]))
	+ scale_fill_manual(values = c("red", "blue"))

	+ theme(axis.title.y = element_text(size = 10))
	+ theme(legend.position = "none")
)

print(stackbar)
