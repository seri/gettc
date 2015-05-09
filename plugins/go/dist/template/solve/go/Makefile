include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/<%= prob.name %>Solver

$(SOLVER): <%= prob.name %>Solver.go <%= prob.name %>/<%= prob.name %>.go
	export GOPATH="$$GOPATH<%= File::PATH_SEPARATOR %>$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) <%= prob.name %>Solver.go 

main: setup $(SOLVER)
