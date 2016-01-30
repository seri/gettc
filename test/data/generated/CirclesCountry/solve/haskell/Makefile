include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/CirclesCountrySolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): CirclesCountrySolver.hs CirclesCountry.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

