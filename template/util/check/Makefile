default : check

check : check.cc
	if [ ! -d ../../build ]; then mkdir ../../build; fi
	g++ -o ../../build/$@ $<
