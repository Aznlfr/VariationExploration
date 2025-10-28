library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tidyr)
library(shellpipes)

loadEnvironments()

startGraphics(width=3, height=4)

res_mat_long <- (res_mat
	|> pivot_longer(
		cols = c(between, within)
		, names_to = "source", values_to = "RcVariance"
	)
	|> filter(kpa %in% c(0, 2))
	|> mutate(
		kLabel = paste0("kappa ==", kpa)
		, B0 = as.factor(B0)
	)
)

stackbar <- (ggplot(res_mat_long)
	+ aes(x = B0, y = RcVariance,  fill = source)
	+ geom_bar(
		stat = "identity",
		position = "stack"
	)
	+ facet_wrap( ~ kLabel, labeller = label_parsed)
	## + facet_wrap(~kpas , labeller = label_bquote(kappa == .(kpas)))

	+ ylab(bquote("Variance in " ~ R[c]))
	+ xlab(bquote(R[0]))
	+ scale_fill_manual(values = c("red", "blue"))

	+ theme(axis.title.y = element_text(size = 10))
	+ theme(legend.position = "none")
)

print(stackbar)
