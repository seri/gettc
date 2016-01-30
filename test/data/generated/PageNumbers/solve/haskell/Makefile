include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/PageNumbersSolver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): PageNumbersSolver.hs PageNumbers.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

