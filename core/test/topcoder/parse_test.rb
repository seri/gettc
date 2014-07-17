require "test/unit" 
require "topcoder/parse" 
include TopCoder

class ParseTest < Test::Unit::TestCase
    def setup
        downloader = Downloader.new Account.new "gettc", "algorithm"
        @parser = Parser.new downloader
        @datadir = File.join File.dirname(__FILE__), "../../files/stats"
    end
    def get_problem_raw prob
        return File.read File.join @datadir, prob + ".htm"
    end
    def test_indexes
        assert_equal [0, 3], @parser.indexes("abcde", "bc")
        assert_equal nil, @parser.indexes("abcde", "f")
    end
    def test_filter
        assert_equal "Lorem Ipsum", @parser.filter("<xml>   Lorem Ipsum  </xml> ")
        assert_equal "2^(3)", @parser.filter("2<sup>3</sup>")
        assert_equal "*hi*", @parser.filter("   <b>hi</b>")
        html = <<-END
            <img src=
            "http://www.topcoder.com/contest/problem/CirclesCountry/case1.gif">
        END
        assert_equal "![image](images/case1.gif)", @parser.filter(html)
    end
    def test_PageNumbers
        html = get_problem_raw "PageNumbers"
        prob = @parser.parse html
        assert_equal "PageNumbers", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 1, prob.notes.size
        assert_equal 1, prob.constraints.size
        assert_equal 5, prob.examples.size
        assert_equal 118, prob.systests.size
        assert_equal 0, prob.images.size
    end
    def test_CirclesCountry
        html = get_problem_raw "CirclesCountry"
        prob = @parser.parse html
        assert_equal "CirclesCountry", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 0, prob.notes.size
        assert_equal 7, prob.constraints.size
        assert_equal 5, prob.examples.size
        assert_equal 228, prob.systests.size
        assert_equal 4, prob.images.size
    end
    def test_TheTournamentDivOne
        html = get_problem_raw "TheTournamentDivOne"
        prob = @parser.parse html
        assert_equal "TheTournamentDivOne", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 0, prob.notes.size
        assert_equal 4, prob.constraints.size
        assert_equal 4, prob.examples.size
        assert_equal 0, prob.systests.size
        assert_equal 0, prob.images.size
    end
    def test_FunnyGames
        html = get_problem_raw "FunnyGames"
        prob = @parser.parse html
        assert_equal "FunnyGames", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 0, prob.notes.size
        assert_equal 6, prob.constraints.size
        assert_equal 5, prob.examples.size
        assert_equal 0, prob.systests.size
        assert_equal 0, prob.images.size
    end
    def test_BuildingRoads
        html = get_problem_raw "BuildingRoads"
        prob = @parser.parse html
        assert_equal "BuildingRoads", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 2, prob.notes.size
        assert_equal 10, prob.constraints.size
        assert_equal 5, prob.examples.size
        assert_equal 0, prob.systests.size
        assert_equal 2, prob.images.size
    end
    def test_Acronyms
        html = get_problem_raw "Acronyms"
        prob = @parser.parse html
        assert_equal "Acronyms", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 1, prob.notes.size
        assert_equal 8, prob.constraints.size
        assert_equal 8, prob.examples.size
        assert_equal 39, prob.systests.size
        assert_equal 0, prob.images.size
    end
    def test_BackyardTrees
        html = get_problem_raw "BackyardTrees"
        prob = @parser.parse html
        assert_equal "BackyardTrees", prob.name
        assert_equal 5, prob.definitions.size
        assert_equal 0, prob.notes.size
        assert_equal 4, prob.constraints.size
        assert_equal 6, prob.examples.size
        assert_equal 2, prob.images.size
    end
end
