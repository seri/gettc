include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/PageNumbersSolver

$(SOLVER): PageNumbersSolver.go PageNumbers/PageNumbers.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) PageNumbersSolver.go 

main: setup $(SOLVER)
