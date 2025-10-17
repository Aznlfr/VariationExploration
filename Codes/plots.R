library(shellpipes)

loadEnvironments()
startGraphics()

par(cex=1.5)

boxCol <- c("black", "blue", "darkgreen")
boxLines <- c(2, 1, 3)
boxWidth <- c(1.5, 1.5, 2.5)

if (!exists("slist"))
	slist <- list(sdat)

for (sdat in slist){

	with(sdat, {
		plot(time, x
			, type="l"
			, ylim = c(0, max(x))
			, lty=boxLines[[1]]
			, lwd=boxWidth[[1]]
			, col = boxCol[[1]]
			, xlab="Time (disease generations)"
			, ylab="Proportion of population"
		)

		legend("topright"
			, legend = c("Susceptible", "Infectious", "Recovered")
			, lty=boxLines
			, lwd=boxWidth
			, col = boxCol
		)

		lines(time, y
			, lty=boxLines[[2]]
			, col=boxCol[[2]]
			, lwd=boxWidth[[2]]
		)

		lines(time, z
			, lty=boxLines[[3]]
			, col=boxCol[[3]]
			, lwd=boxWidth[[3]]
		)
	})
}

firstPlot <- TRUE
for (sdat in slist){
	arrP <- round(nrow(sdat)/5)
	if(firstPlot){
		with(sdat, {plot(x, y, type="l"
			, xlab="Proportion susceptible"
			, ylab="Proportion infectious"
		)})
		firstPlot <<- FALSE
	}
	with(sdat,{
		lines(x, y)
		arrows(x[arrP], y[arrP], x[1+arrP], y[1+arrP])
	})
}
