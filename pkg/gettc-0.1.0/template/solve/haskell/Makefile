BUILD_D=../../build/haskell
DATA_D=../../data
OUTPUT_D=$(BUILD_D)
RUNNER=sh ../../bin/runner.sh
HS=ghc -i$$HASKELL_INCLUDE_PATH -outputdir $(BUILD_D)

run = $(RUNNER) $1 $(BUILD_D)/solve $(DATA_D)/$2 $(OUTPUT_D)/$2

default : demo

setup :
	if [ ! -d $(BUILD_D) ]; then mkdir -p $(BUILD_D); fi

main : Main.hs Solve.hs setup
	$(HS) -o $(BUILD_D)/solve $<

test : Test.hs Solve.hs setup
	$(HS) -o $(BUILD_D)/test $<
	$(BUILD_D)/test

demo : main
	$(call run,verbose,demo) 

sys : main
	$(call run,quiet,sys)

sysv : main
	$(call run,verbose,sys)

clean :
	rm -rf $(BUILD_D)
