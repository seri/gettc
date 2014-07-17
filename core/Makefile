

setup:
	mkdir files
	mkdir files/yamls
	mkdir files/stats
	mkdir files/details
	mkdir files/solutions
clean:
	rm -rf files

download:
	ruby -Ilib inspect/topcoder/download_inspect.rb
parse:
	ruby -Ilib inspect/topcoder/parse_inspect.rb
generate:
	ruby -Ilib inspect/topcoder/generate_inspect.rb

test_download:
	ruby -Ilib test/topcoder/download_test.rb
test_parse: download
	ruby -Ilib test/topcoder/parse_test.rb


test: test_download test_parse test_generate