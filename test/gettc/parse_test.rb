require "test/unit"
require "gettc/parse"
include Gettc

class FakeDownloader
  def initialize(problem_name = nil)
    @problem_name = problem_name
  end

  def download_statement(problem_id)
    read_problem(:statement, @problem_name)
  end

  def download_detail(problem_id, round_id)
    read_problem(:detail, @problem_name)
  end

  def download_solution(problem_id, round_id, solution_id)
    read_problem(:solution, @problem_name)
  end
end

class ParseTest < Test::Unit::TestCase
  def setup
    @downloader = FakeDownloader.new
    @parser = Parser.new(@downloader)
  end

  def test_indexes
    assert_equal [0, 3], @parser.send(:indexes, "abcde", "bc")
    assert_equal nil, @parser.send(:indexes, "abcde", "f")
  end

  def test_filter
    assert_equal "Lorem Ipsum", @parser.send(:filter, "<xml>   Lorem Ipsum  </xml> ")
    assert_equal "2^(3)", @parser.send(:filter, "2<sup>3</sup>")
    assert_equal "*hi*", @parser.send(:filter, "   <b>hi</b>")

    html = <<-END
      <img src=
      "http://www.topcoder.com/contest/problem/CirclesCountry/case1.gif">
    END
    assert_equal "![image](images/case1.gif)", @parser.send(:filter, html)
  end

  def test_parse_problems
    $problems.each do |hash|
      downloader = FakeDownloader.new(hash["name"])
      problem = Parser.new(downloader).parse(downloader.download_statement(nil))
      assert_equal hash["name"]
    end
  end

  #def test_PageNumbers
    #html = get_problem_raw "PageNumbers"
    #prob = @parser.parse html
    #assert_equal "PageNumbers", prob.name
    #assert_equal 5, prob.definitions.size
    #assert_equal 1, prob.notes.size
    #assert_equal 1, prob.constraints.size
    #assert_equal 5, prob.examples.size
    #assert_equal 118, prob.systests.size
    #assert_equal 0, prob.images.size
  #end

  #def test_CirclesCountry
    #@parser = Parser.new(@downloader = FakeDownloader.new("CirclesCountry"))
    #problem = @parser.parse(@downloader.get_statement_html)

    #assert_equal "CirclesCountry", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 0, problem.notes.size
    #assert_equal 7, problem.constraints.size
    #assert_equal 5, problem.examples.size
    #assert_equal 228, problem.systests.size
    #assert_equal 4, problem.images.size
  #end

  #def test_TheTournamentDivOne
    #html = get_problem_raw "TheTournamentDivOne"
    #problem = @parser.parse html
    #assert_equal "TheTournamentDivOne", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 0, problem.notes.size
    #assert_equal 4, problem.constraints.size
    #assert_equal 4, problem.examples.size
    #assert_equal 0, problem.systests.size
    #assert_equal 0, problem.images.size
  #end

  #def test_FunnyGames
    #html = get_problem_raw "FunnyGames"
    #problem = @parser.parse html
    #assert_equal "FunnyGames", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 0, problem.notes.size
    #assert_equal 6, problem.constraints.size
    #assert_equal 5, problem.examples.size
    #assert_equal 0, problem.systests.size
    #assert_equal 0, problem.images.size
  #end

  #def test_BuildingRoads
    #html = get_problem_raw "BuildingRoads"
    #problem = @parser.parse html
    #assert_equal "BuildingRoads", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 2, problem.notes.size
    #assert_equal 10, problem.constraints.size
    #assert_equal 5, problem.examples.size
    #assert_equal 0, problem.systests.size
    #assert_equal 2, problem.images.size
  #end

  #def test_Acronyms
    #html = get_problem_raw "Acronyms"
    #problem = @parser.parse html
    #assert_equal "Acronyms", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 1, problem.notes.size
    #assert_equal 8, problem.constraints.size
    #assert_equal 8, problem.examples.size
    #assert_equal 39, problem.systests.size
    #assert_equal 0, problem.images.size
  #end

  #def test_BackyardTrees
    #html = get_problem_raw "BackyardTrees"
    #problem = @parser.parse html
    #assert_equal "BackyardTrees", problem.name
    #assert_equal 5, problem.definitions.size
    #assert_equal 0, problem.notes.size
    #assert_equal 4, problem.constraints.size
    #assert_equal 6, problem.examples.size
    #assert_equal 2, problem.images.size
  #end
end
