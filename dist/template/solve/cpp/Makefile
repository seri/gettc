GETTC_HOME ?= $(HOME)/.gettc
OUTPUT_D =../../build/cpp
DATA_D = ../../data
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp
RUNNER = sh ../../bin/runner.sh

run = @$(RUNNER) $1 $(OUTPUT_D)/<%= prob.name %>Runner $(DATA_D)/$2 $(OUTPUT_D)/$2

default : usage

usage :
	@cat $(GETTC_HOME)/usage

setup :
	@if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi
	@echo "[gettc] Compile solver"

$(OUTPUT_D)/<%= prob.name %>Runner: <%= prob.name %>Runner.cpp <%= prob.name %>.cpp
	$(COMPILER) -o $(OUTPUT_D)/<%= prob.name %>Runner $<

main: setup $(OUTPUT_D)/<%= prob.name %>Runner

demo : main
	$(call run,verbose,demo) 

sys : main
	$(call run,quiet,sys)

sysv : main
	$(call run,verbose,sys)

clean :
	@rm -rf $(OUTPUT_D)
