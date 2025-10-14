## This is Azadeh's VariationExploration repo
## Related to Roswell conjecture, etc.

current: target
-include target.mk
Ignore = target.mk

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.tex)
## moments.pdf: moments.tex

######################################################################

Sources += $(wildcard Codes/*.R)

autopipeR = defined
Codes/RcStat.Rout: Codes/RcStat.R
	$(pipeRcall)

## Codes/stackBarPlot.Rout: Codes/stackBarPlot.R
Codes/%.Rout: Codes/%.R Codes/RcStat.Rout Codes/RcStat.rda
	$(pipeRcall)

######################################################################

Sources += $(wildcard slow/*)
slowtarget/multiSim.Rout: Codes/multiSim.R Codes/RcStat.rda
	$(pipeR)

Ignore += figs
figs/bars.Rout: slow/multiSim.rda Codes/bars.R | figs
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
