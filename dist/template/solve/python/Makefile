GETTC_HOME ?= $(HOME)/.gettc
OUTPUT_D = ../../build/python
DATA_D = ../../data
RUNNER = sh ../../bin/runner.sh

run = $(RUNNER) $1 ./<%= prob.name %>Runner.py $(DATA_D)/$2 $(OUTPUT_D)/$2

setup :
	if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi
	chmod +x <%= prob.name %>Runner.py

default : usage

usage :
	@cat $(GETTC_HOME)/usage

demo : setup
	$(call run,verbose,demo) 

sys : setup
	$(call run,quiet,sys)

sysv : setup
	$(call run,verbose,sys)

clean :
	rm -rf $(OUTPUT_D)
