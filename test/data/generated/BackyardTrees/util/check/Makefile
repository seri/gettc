GETTC_HOME ?= $(HOME)/.gettc

default : check

check : check.cpp
	if [ ! -d ../../build ]; then mkdir ../../build; fi
	g++ -I$(GETTC_HOME)/include/cpp -o ../../build/$@ $<
