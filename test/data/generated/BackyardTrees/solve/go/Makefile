include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/BackyardTreesSolver

$(SOLVER): BackyardTreesSolver.go BackyardTrees/BackyardTrees.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) BackyardTreesSolver.go 

main: setup $(SOLVER)
