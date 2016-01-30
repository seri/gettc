include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/BuildingRoadsSolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): BuildingRoadsSolver.hs BuildingRoads.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

