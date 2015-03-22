GETTC_HOME ?= <%= File.expand_path "~/.gettc" %>
OUTPUT_D =../../build/go
DATA_D = ../../data
RUNNER = sh ../../bin/runner.sh

run = @$(RUNNER) $1 $(OUTPUT_D)/<%= prob.name %>Runner $(DATA_D)/$2 $(OUTPUT_D)/$2

default : usage

usage :
	@cat $(GETTC_HOME)/usage

setup :
	@if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi
	@echo "[gettc] Compile solver"

$(OUTPUT_D)/<%= prob.name %>Runner: <%= prob.name %>Runner.go <%= prob.name %>/<%= prob.name %>.go
	export GOPATH="$$GOPATH<%= File::PATH_SEPARATOR %>$(GETTC_HOME)/include/go" && go build -o $(OUTPUT_D)/<%= prob.name %>Runner <%= prob.name %>Runner.go 

main: setup $(OUTPUT_D)/<%= prob.name %>Runner

demo : main
	$(call run,verbose,demo) 

sys : main
	$(call run,quiet,sys)

sysv : main
	$(call run,verbose,sys)

clean :
	@rm -rf $(OUTPUT_D)
