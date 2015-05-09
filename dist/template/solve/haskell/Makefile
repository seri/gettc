include ../Makefile.common

OUTPUT_D = ../../build/haskell
SOLVER = $(OUTPUT_D)/<%= prob.name %>Solver
COMPILER = ghc -i$(GETTC_HOME)/include/haskell -outputdir $(OUTPUT_D)

$(SOLVER): <%= prob.name %>Solver.hs <%= prob.name %>.hs
	$(COMPILER) -o $(SOLVER) $<

main: setup $(SOLVER)

