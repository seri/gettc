include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/CirclesCountrySolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): CirclesCountrySolver.cpp CirclesCountry.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
