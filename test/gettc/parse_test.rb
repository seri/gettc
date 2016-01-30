require "test_helper"
require "gettc/parse"

class FakeDownloader
  def initialize(problem_name = nil)
    @problem_name = problem_name
  end

  def download(url)
    "ImageContent"
  end

  def download_statement(problem_id)
    read_problem_html(:statement, @problem_name)
  end

  def download_detail(problem_id, round_id)
    read_problem_html(:detail, @problem_name)
  end

  def download_solution(problem_id, round_id, solution_id)
    read_problem_html(:solution, @problem_name)
  end

end

class ParseTest < Test::Unit::TestCase
  def test_get_param_value
    assert_equal 13751, Parser.send(:get_param_value,
      "/tc?module=ProblemDetail&rd=13751&pm=10297", "rd").to_i
    assert Parser.send(:get_param_value,
      "/tc?module=ProblemDetail&rd=13751&pm=10297", "cr").nil?
    assert_equal 22717415, Parser.send(:get_param_value,
      "/stat?c=problem_solution&amp;cr=22717415&amp;rd=13751&amp;pm=10297", "cr").to_i
  end

  def test_indexes
    assert_equal [0, 3], Parser.send(:indexes, "abcde", "bc")
    assert_equal nil, Parser.send(:indexes, "abcde", "f")
  end

  def test_filter
    parser = Parser.new(FakeDownloader.new(nil))

    assert_equal "Lorem Ipsum", parser.send(:filter, "<xml>   Lorem Ipsum  </xml> ")
    assert_equal "2^(3)", parser.send(:filter, "2<sup>3</sup>")
    assert_equal "*hi*", parser.send(:filter, "   <b>hi</b>")

    html = <<-END
      <img src=
      "http://www.topcoder.com/contest/problem/CirclesCountry/case1.gif">
    END
    assert_equal "![image](images/case1.gif)", parser.send(:filter, html)
  end

  def test_parse_problems
    $problems.each do |hash|
      downloader = FakeDownloader.new(hash["name"])
      problem = Parser.new(downloader).parse(hash["id"])

      assert_equal hash["name"], problem.name, hash["message"]
      assert_equal hash["source"], problem.source, hash["message"]
      assert_equal hash["definitions_count"], problem.definitions.size, hash["message"]
      assert_equal hash["notes_count"], problem.notes.size, hash["message"]
      assert_equal hash["contraints_count"], problem.constraints.size, hash["message"]
      assert_equal hash["examples_count"], problem.examples.size, hash["message"]
      assert_equal hash["systests_count"], problem.systests.size, hash["message"]
      assert_equal hash["images_count"], problem.images.size, hash["message"]

      (problem.examples + problem.systests).each do |test_case|
        assert !test_case.input.empty?
        assert !test_case.output.empty?
      end

      write_problem_yaml(problem)
    end
  end
end
