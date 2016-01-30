include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/TheTournamentDivOneSolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): TheTournamentDivOneSolver.cpp TheTournamentDivOne.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
