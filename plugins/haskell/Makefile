OUTPUT_D=../../dist/include/haskell

hs : test.hs TopCoder.hs
	ghc --make -o test $<
	./test
	rm -f test{,.hi,.o,.exe} TopCoder.{hi,o}

rb :
	ruby -I../../core/lib -I. test.rb

dist :
	if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi
	cp TopCoder.hs engine.rb $(OUTPUT_D)

clean :
	rm *.{hi,o}
