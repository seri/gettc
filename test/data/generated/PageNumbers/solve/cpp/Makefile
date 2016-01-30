include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/PageNumbersSolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): PageNumbersSolver.cpp PageNumbers.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
