## This is Azadeh's VariationExploration repo
## Related to Roswell conjecture, etc.

current: target
-include target.mk
Ignore = target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.tex *.bib)
moments.pdf: moments.tex
within_btw_Rc.pdf: within_btw_Rc.tex
######################################################################

Sources += $(wildcard Codes/*.R)

autopipeR = defined
Codes/RcStat.Rout: Codes/RcStat.R
	$(pipeRcall)

## Codes/stackBarPlot.Rout: Codes/stackBarPlot.R
Codes/%.Rout: Codes/%.R Codes/RcStat.Rout Codes/RcStat.rda
	$(pipeRcall)
Codes/RcStat_v2.Rout: Codes/RcStat_v2.R
	$(pipeRcall)
Codes/GIstatistics.Rout: Codes/GIstatistics.R
	$(pipeR)
######################################################################

Sources += $(wildcard slow/*)
slowtarget/multiSim.Rout: Codes/multiSim.R Codes/RcStat.rda
	$(pipeR)

slowtarget/timePlotSim.Rout: Codes/timePlotSim.R Codes/RcStat.rda
	$(pipeR)

slowtarget/RcbarPlotVaryingSigmaSim.Rout: Codes/RcbarPlotVaryingSigmaSim.R Codes/RcStat.rda
	$(pipeR)

slowtarget/RcbarPlotVaryingKappaSim.Rout: Codes/RcbarPlotVaryingKappaSim.R Codes/RcStat.rda
	$(pipeR)

slowtarget/ErlangMomentsSim.Rout: Codes/ErlangMomentsSim.R Codes/RcStat.rda
	$(pipeR)

slowtarget/Rcstat_v2Sim.Rout: Codes/Rcstat_v2Sim.R Codes/RcStat_v2.rda
	$(pipeR)

slowtarget/RiHigherMomentsSim.Rout: Codes/RiHigherMomentsSim.R Codes/RcStat.rda
	$(pipeR)

Ignore += figs
figs/bars.Rout: slow/multiSim.rda Codes/bars.R | figs
	$(pipeR)

figs/ErlangMoments.Rout: slow/ErlangMomentsSim.rda Codes/ErlangMoments.R | figs
	$(pipeR)

figs/RiMoments.Rout: slow/ErlangMomentsSim.rda Codes/RiMoments.R | figs
	$(pipeR)

figs/timePlot.Rout: slow/timePlotSim.rda Codes/timePlot.R | figs
	$(pipeR)

figs/RcbarPlotVaryingSigma.Rout: slow/RcbarPlotVaryingSigmaSim.rda Codes/RcbarPlotVaryingSigma.R | figs
	$(pipeR)

figs/RibarPlotVaryingSigma.Rout: slow/RcbarPlotVaryingSigmaSim.rda Codes/RibarPlotVaryingSigma.R | figs
	$(pipeR)

figs/RcbarPlotVaryingKappa.Rout: slow/RcbarPlotVaryingKappaSim.rda Codes/RcbarPlotVaryingKappa.R | figs
	$(pipeR)

figs/Rcstat_v2.Rout: slow/Rcstat_v2Sim.rda Codes/Rcstat_v2Plot.R | figs
	$(pipeR)

figs/RiHigherMoments.Rout: slow/RiHigherMomentsSim.rda Codes/RiHigherMoments.R | figs
	$(pipeR)

figs:
	$(mkdir)

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/01.stamp
makestuff/%.stamp: | makestuff
	- $(RM) makestuff/*.stamp
	cd makestuff && $(MAKE) pull
	touch $@
makestuff:
	git clone --depth 1 $(msrepo)/makestuff

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texj.mk
-include makestuff/slowtarget.mk

-include makestuff/git.mk
-include makestuff/visual.mk
