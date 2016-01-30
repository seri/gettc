include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/CirclesCountrySolver

$(SOLVER): CirclesCountrySolver.go CirclesCountry/CirclesCountry.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) CirclesCountrySolver.go 

main: setup $(SOLVER)
