include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/BackyardTreesSolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): BackyardTreesSolver.cpp BackyardTrees.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
