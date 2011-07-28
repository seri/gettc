CC=g++ -std=gnu++0x
BUILD_D=../../build/cpp
DATA_D=../../data
OUTPUT_D=$(BUILD_D)
RUNNER=sh ../../bin/runner.sh

run = $(RUNNER) $1 $(BUILD_D)/solve $(DATA_D)/$2 $(OUTPUT_D)/$2

default : demo

setup :
	if [ ! -d $(BUILD_D) ]; then mkdir -p $(BUILD_D); fi

main : main.cc solve.hh setup
	$(CC) -o $(BUILD_D)/solve $<

test : test.cc solve.hh setup
	$(CC) -lboost_unit_test_framework -o $(BUILD_D)/test $<
	$(BUILD_D)/test

demo : main
	$(call run,-v,demo) 

sys : main
	$(call run,-q,sys)

clean :
	rm -rf $(BUILD_D)
