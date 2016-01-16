include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/<%= prob.name %>Solver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): <%= prob.name %>Solver.cpp <%= prob.name %>.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
