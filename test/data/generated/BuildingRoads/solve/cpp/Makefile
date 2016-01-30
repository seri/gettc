include ../Makefile.common

OUTPUT_D =../../build/cpp
SOLVER = $(OUTPUT_D)/BuildingRoadsSolver
COMPILER = g++ -std=gnu++0x -I$(GETTC_HOME)/include/cpp

$(SOLVER): BuildingRoadsSolver.cpp BuildingRoads.cpp
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)
