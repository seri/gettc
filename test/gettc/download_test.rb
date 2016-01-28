require "test_helper"
require "gettc/download"

class DownloadTest < Test::Unit::TestCase
  def setup
    @downloader = Downloader.new($account)
  end

  def teardown
    Dir.glob("gettc_http_response*.html").each { |filename| File.delete(filename) }
  end

  def test_wrong_account
    assert_raises AuthTokenError do
      Downloader.new(Account.new("username", "password"))
    end
  end

  def test_download_statement
    $problems.each do |hash|
      html = @downloader.download_statement(hash["id"])
      assert_match "<h3>Problem Statement</h3>", html
      write_problem(:statement, hash["name"], html)
    end
  end

  def test_download_statement_wrong_problem_id
    assert_raises IDNotAvailable do
      @downloader.download_statement(1000000)
    end
  end

  def test_download_detail
    $problems.each do |hash|
      html = @downloader.download_detail(hash["id"], hash["round_id"])
      assert_match "Problem Detail", html
      write_problem(:detail, hash["name"], html)
    end
  end

  def test_download_detail_wrong_round_id
    assert_raises HttpError do
      @downloader.download_detail(10297, 1000000)
    end
  end

  def test_download_solution
    $problems.each do |hash|
      html = @downloader.download_solution(hash["id"], hash["round_id"], hash["solution_id"])
      assert_match "TopCoder Statistics - Problem Solution", html
      write_problem(:solution, hash["name"], html)
    end
  end

  def test_download_solution_wrong_solution_id
    assert_raises BadResponseError do
      @downloader.download_solution(10297, 13751, 1000000)
    end
  end
end
