include ../Makefile.common

OUTPUT_D =../../build/go
SOLVER = $(OUTPUT_D)/BuildingRoadsSolver

$(SOLVER): BuildingRoadsSolver.go BuildingRoads/BuildingRoads.go
	export GOPATH="$$GOPATH:$(GETTC_HOME)/include/go" \
		&& go build -o $(SOLVER) BuildingRoadsSolver.go 

main: setup $(SOLVER)
