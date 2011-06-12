default: cc

test: test.cc
	g++ test.cc -o test -std=gnu++0x

cc: solve.cc test
	g++ solve.cc -o out.cc/solve -std=gnu++0x
	sh test.sh out.cc

hs: solve.hs test
	ghc solve.hs -o out.hs/solve
	sh test.sh out.hs
	rm test.{hi,o}

clean:
	rm -f out.cc/* out.hs/* test test.exe
