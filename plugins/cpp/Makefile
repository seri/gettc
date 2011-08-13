OUTPUT_D=../../dist/include/cpp

cc : test.cc topcoder
	g++ -o test $< -lboost_unit_test_framework
	./test
	rm test

rb :
	ruby -I../../core/lib -I. test.rb

dist : 
	if [ ! -d $(OUTPUT_D) ]; then mkdir -p $(OUTPUT_D); fi
	cp topcoder engine.rb $(OUTPUT_D)