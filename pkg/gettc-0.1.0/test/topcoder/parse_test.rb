require 'test/unit' 
require 'topcoder/parse' 
include TopCoder

class ParseTest < Test::Unit::TestCase
    def setup
        @parser = Parser.new
        @datadir = File.join File.dirname(__FILE__), 'files'
    end
    def get_problem_raw prob
        return File.read File.join(@datadir, prob + '.htm')
    end
    def test_indexes
        assert_equal [0, 3], @parser.indexes('abcde', 'bc')
        assert_equal nil, @parser.indexes('abcde', 'f')
    end
    def test_filter
        assert_equal 'Lorem Ipsum', @parser.filter('<xml>   Lorem Ipsum  </xml> ')
        assert_equal '2^(3)', @parser.filter('2<sup>3</sup>')
        assert_equal '*hi*', @parser.filter('   <b>hi</b>')
        html = <<-END
            <img src=
            "http://www.topcoder.com/contest/problem/CirclesCountry/case1.gif">
        END
        assert_equal '![image](images/case1.gif)', @parser.filter(html)
    end
    def test_prob_PageNumbers
        html = get_problem_raw 'PageNumbers'
        prob, images = @parser.parse html
        assert_equal 0, images.size
        assert_equal 'PageNumbers', prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 1, prob.notes.size
        assert_equal 1, prob.constraints.size
        assert_equal 5, prob.examples.size
    end
    def test_prob_CirclesCountry
        html = get_problem_raw 'CirclesCountry'
        prob, images = @parser.parse html
        assert_equal 4, images.size
        assert_equal 'CirclesCountry', prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 0, prob.notes.size
        assert_equal 7, prob.constraints.size
        assert_equal 5, prob.examples.size
    end    
end
