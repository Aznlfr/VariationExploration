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

slowtarget/multiSim.Rout: Codes/multiSim.R Codes/RcStat.rda
	$(pipeR)

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

## ln -s ../makestuff . ## Do this first if you want a linked makestuff
Makefile: makestuff/00.stamp
makestuff/%.stamp: | makestuff
	- $(RM) makestuff/*.stamp
	cd makestuff && $(MAKE) pull
	touch $@
makestuff:
	git clone --depth 1 $(msrepo)/makestuff

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texj.mk

-include makestuff/git.mk
-include makestuff/visual.mk
