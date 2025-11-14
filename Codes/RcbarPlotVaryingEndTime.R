library(ggplot2); theme_set(theme_bw() +theme(
  plot.background = element_blank(),
  panel.grid = element_blank(),
) )
library(dplyr)
library(patchwork)
library(tidyr)
library(deSolve)
library(purrr)
library(shellpipes)

loadEnvironments()
startGraphics(width=10, height=10)




wrap_f <- function(wrap_level, B0) {
  sapply(wrap_level,function(x){
  paste("Cohorts infected prior to time", x, "are considered")
  })}
lineType <- "dotted"
interceptLineWidth <- 0.5 #intercept's line width
LineWidth <-1 #lines' line width
############### Bar Plot ########################
stackbar<-( res_mat    |> pivot_longer(cols = c(between, within),
                                       names_to = "source",
                                       values_to = "RcVariance")
            |> mutate(cutoffTime = factor(cutoffTime
                      , levels = sort(unique(cutoffTime)))
                      , B0 = as.factor(B0) )|>
              ggplot()
            + aes(x = B0, y = RcVariance,  fill = source)
            +  geom_bar(stat = "identity", position = "stack")
            + geom_hline (yintercept = 1, linetype=lineType
                          , linewidth=interceptLineWidth)
            + facet_wrap(~ cutoffTime
                         , labeller = labeller(cutoffTime = function(x){
                wrap_f(x)
              })
              )
            + scale_color_viridis_d()
            +  ylab("Variance in Rc")
            + xlab(bquote(beta))
            + theme(axis.title.y = element_text(size = 10)
                    ,strip.text.x = element_text(size = 8))
)

########### Rc and kappa_c over time #########
colorVec<- c("#F8766D", "#00BFC4", "#7B3294", "#E69F00", "#56B4E9"
             ,"#009E73", "#0072B2", "#D55E00", "#CC79A7")
cohortXlabel <- "cohort infection time"
rc <- (cohorts |> ggplot(aes(cohort, Rc, color = as.factor(B0))) 
  + geom_line(linewidth = LineWidth) 
  + geom_vline(xintercept = cutoffTime, linetype=lineType
               , linewidth=interceptLineWidth)
  + scale_color_manual(values = colorVec)
  + guides(color = "none") 
  + xlim(c(0, temporalFinalTime)) 
  + labs(x = cohortXlabel
       , y = "mean cohort \ncases per case"
       , color = bquote(beta)))


kc <- (cohorts |>
  mutate(kappa_c = varRc/Rc^2) |>
  ggplot(aes(cohort, kappa_c, color = as.factor(B0))) 
  + geom_line(linewidth = LineWidth) 
  + geom_vline(xintercept = cutoffTime
               , linetype=lineType
               , linewidth=interceptLineWidth)
    + scale_color_manual(values = colorVec[1:length(B0)])
   + xlim(c(0, temporalFinalTime)) 
  + guides(color="none") +
  labs(x = cohortXlabel
       ,y = bquote(kappa[c])
       , color = bquote(beta) )
  )

cohortFig <- rc + kc
############### Time Evolution ##############
epiXlabel <- "normalized time"
SusPlot  <- (straightSim |>
               ggplot(aes(time, x, color = as.factor(B0)))
             + geom_line(linewidth = LineWidth)
             + geom_vline(xintercept = cutoffTime
                          , linetype=lineType
                          , linewidth=interceptLineWidth)
             + scale_color_manual(values = colorVec)
             +  labs(x = epiXlabel
                     , y = "population proportion \n of susptibles"
                     , color = bquote(beta))
             +
               xlim(c(0, temporalFinalTime))
             +  guides(color="none")
)

inc <- (straightSim |>
          ggplot(aes(time, inc, color = as.factor(B0)))
        + geom_line(linewidth = LineWidth)
        + geom_vline(xintercept = cutoffTime
                     , linetype=lineType
                     , linewidth=interceptLineWidth)
        + scale_color_manual(values = colorVec[1:length(B0)])
        + labs(x = epiXlabel
               , y = "incidence"
               , color = bquote(beta))
        + xlim(c(0, temporalFinalTime)) 
        
        
)
epiFig<- inc + SusPlot
############### Final Plot #############
print(stackbar / epiFig / cohortFig
      + plot_annotation(tag_levels ="a")
      + plot_layout(heights=c(2,1,1) )
 )


#saveEnvironment()
