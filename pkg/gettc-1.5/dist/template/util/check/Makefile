default : check

check : check.cpp
	if [ ! -d ../../build ]; then mkdir ../../build; fi
	g++ -I$$HOME/.gettc/include/cpp -o ../../build/$@ $<
