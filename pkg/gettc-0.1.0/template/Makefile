test: test.cc
	g++ -std=gnu++0x -o test test.cc 

cc: solve.cc test
	if [ ! -d out.cc ]; then mkdir out.cc; fi
	g++ -std=gnu++0x -o out.cc/solve solve.cc 
	sh test.sh out.cc

hs: solve.hs test
	if [ ! -d out.hs ]; then mkdir out.hs; fi
	ghc -i$$HASKELL_INCLUDE_PATH -o out.hs/solve solve.hs 
	sh test.sh out.hs
	rm solve.{hi,o}

clean:
	rm -rf out.cc out.hs test test.exe
