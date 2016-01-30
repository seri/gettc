include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/BackyardTreesSolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): BackyardTreesSolver.hs BackyardTrees.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

