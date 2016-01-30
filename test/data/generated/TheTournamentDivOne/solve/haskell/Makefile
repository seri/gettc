include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/TheTournamentDivOneSolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): TheTournamentDivOneSolver.hs TheTournamentDivOne.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

