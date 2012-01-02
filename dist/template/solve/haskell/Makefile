OUTPUT_D=../../build/haskell
DATA_D=../../data
COMPILER=ghc -i$$HOME/.gettc/include/haskell -outputdir $(OUTPUT_D)
RUNNER=sh ../../bin/runner.sh

run = $(RUNNER) $1 $(OUTPUT_D)/<%= prob.name %>Runner $(DATA_D)/$2 $(OUTPUT_D)/$2

default : demo

setup :
	if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi

main : <%= prob.name %>Runner.hs <%= prob.name %>.hs setup
	$(COMPILER) -o $(OUTPUT_D)/<%= prob.name %>Runner $<

test : <%= prob.name %>Test.hs <%= prob.name %>.hs setup
	$(COMPILER) -o $(OUTPUT_D)/<%= prob.name %>Test $<
	$(OUTPUT_D)/<%= prob.name %>Test

demo : main
	$(call run,verbose,demo) 

sys : main
	$(call run,quiet,sys)

sysv : main
	$(call run,verbose,sys)

clean :
	rm -rf $(OUTPUT_D)