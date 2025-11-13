library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(patchwork)
library(tidyr)
library(deSolve)
library(purrr)
library(shellpipes)

loadEnvironments()

startGraphics(width=10, height=10)

infected_so_far <- map_dfr(B0, function(b){
  filtered<-straightSim |> filter(B0==b) 
  sfun = approxfun(filtered$time, filtered$x, rule=2)
  return(data.frame(B0 = b
                    , finTime = finTime, S = sfun(finTime)
                    , Sinf = filtered$x[which.max(filtered$time)]
                    , S2Sinf = sfun(finTime)/ filtered$x[which.max(filtered$time)]))})

wrap_f <- function(wrap_level, B0) {
  sapply(wrap_level,function(x){af<-infected_so_far|>filter(finTime ==x)
  pairs <- with(af, paste0("(", B0, ", ", round(S2Sinf,2), ")"))
  paste(
    paste("Final Time:", x, sep=" "),
    "(beta, x/xinf):",
    paste(pairs, collapse = ", "),
    sep = "\n"
  )
  })}
############### Bar Plot ########################
stackbar<-( res_mat    |> pivot_longer(cols = c(between, within),
                                       names_to = "source",
                                       values_to = "RcVariance")
            |> mutate(finTime = factor(finTime,levels = sort(unique(finTime)))
                      , B0 = as.factor(B0) )|>
              ggplot()
            + aes(x = B0, y = RcVariance,  fill = source)
            +  geom_bar(stat = "identity", position = "stack")
            +
              facet_wrap(~ finTime, labeller = labeller(finTime = function(x){
                wrap_f(x)
              })
              )
            + scale_color_viridis_d()

            +  ylab("Variance in Rc")
            + xlab(bquote(beta))
            + theme(axis.title.y = element_text(size = 10))
)


# barPlotRcTotal<-(res_mat |> mutate(finTime = as.factor(finTime))|>
#                    ggplot()
#                  + aes(x = B0, y = totalVRc, fill = finTime)
#                  +  geom_bar(stat="identity",  position=position_dodge())
#                  +  labs( y = "Variance in Rc"
#                           , x = bquote(beta[0])
#                           , fill = "Stop time")
#                  + scale_color_viridis_d()
#                  + theme(axis.title.y = element_text(size = 10)))


Rc_var_plot_inc<-stackbar 
########### Rc and kappa_c over time #########
cohorts <- map_dfr(B0, function(B0){
  return(data.frame(sapply(cohortStatsRcPlot(kpa = kpas
                                             , omega=omega
                                             , B0=B0
                                             , alpha = alpha
                                             , steps=steps
                                             , cars = cars
                                             , sigma = sigma
                                             , finTime = temporalFinalTime
                                             , y0 = y0
                                             , t0=t0
                                             , cohortProp = cohortProp
                                             ),
                           unlist), B0 = B0))
}
)

rc <- (cohorts |> ggplot(aes(cohort, Rc, color = as.factor(B0))) 
  + geom_line(linewidth = 1, alpha = 0.8) 
  + geom_vline(xintercept = finTime, linetype="dashed", linewidth=0.7)
   + theme_bw() 
  + scale_color_viridis_d(name = bquote(beta[0])) 
  + guides(color = "none") 
  + xlim(c(0, temporalFinalTime)) 
  + labs(x = "cohort infection time"
       , y = "mean cohort \ncases per case"))


kc <- (cohorts |>
  mutate(kappa_c = varRc/Rc^2) |>
  ggplot(aes(cohort, kappa_c, color = as.factor(B0))) +
  geom_line(linewidth = 1, alpha = 0.8) 
  + geom_vline(xintercept = finTime, linetype="dashed", linewidth=0.7)
  + theme_bw() +
  scale_color_viridis_d(name = bquote(beta[0])) 
   + xlim(c(0, temporalFinalTime)) 
  + guides(color="none") +
  labs(x = "cohort infection time"
       ,y = bquote(kappa[c]~"for cohort cases per case")))

cohortFig <- rc + kc
############### Time Evolution ##############
SusPlot  <- (straightSim |>
               ggplot(aes(time, x, color = as.factor(B0)))
             + geom_line(linewidth = 1)
             + geom_vline(xintercept = finTime, linetype="dashed", linewidth=0.7)
             +  scale_color_viridis_d()
             +  labs(x = "normalized time"
                     , y = "population proportion \n of susptibles"
                     , color = bquote(beta))
             +
               xlim(c(0, temporalFinalTime))
             +  guides(color="none")
)

IPlot  <- (straightSim |>
             ggplot(aes(time, y, color = as.factor(B0)))
           + geom_line(linewidth = 1)
           + geom_vline(xintercept = finTime, linetype="dashed", linewidth=0.7)
           + scale_color_viridis_d()
           + labs(x = "noramlized time"
                  , y = "population proportion \n of infectious individuals"
                  , color = bquote(beta))
           + xlim(c(0, temporalFinalTime)) 
           + theme(legend.position = "inside"
                   , legend.position.inside = c(0.5, 0.5))
)
epiFig<- IPlot + SusPlot
############### Final Plot #############
print(Rc_var_plot_inc / epiFig / cohortFig
      + plot_annotation(tag_levels ="a")
      + plot_annotation(title = paste0("Cohorts infected at or before "
                                       , cohortProp*100," % of the final time were included in the simulation"))
)

#saveEnvironment()
