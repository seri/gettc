include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/TheTournamentDivOneSolver

$(SOLVER): TheTournamentDivOneSolver.go TheTournamentDivOne/TheTournamentDivOne.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) TheTournamentDivOneSolver.go 

main: setup $(SOLVER)
