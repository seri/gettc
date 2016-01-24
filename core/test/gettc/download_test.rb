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
    $problems.each do |problem_hash|
      html = @downloader.download_statement(problem_hash["id"])
      assert_match "<h3>Problem Statement</h3>", html
      write_problem_statement(problem_hash["name"], html)
    end
  end

  #def test_download_statement_wrong_problem_id
    #assert_raises IDNotAvailable do
      #@downloader.download_statement(1000000)
    #end
  #end

  #def test_download_detail
    #[ [ 10297, 13751 ],
      #[ 10329, 13757 ],
      #[ 10685, 13907 ],
      #[ 11120, 14285 ],
      #[ 10737, 14153 ],
      #[ 4589,  7225  ],
      #[ 6620,  10008 ],
      #[ 520,   5852  ]
    #].shuffle.take(3).each do |array|
      #assert_match "Problem Detail", @downloader.download_detail(array[0], array[1])
    #end
  #end

  #def test_download_detail_wrong_round_id
    #assert_raises HttpError do
      #@downloader.download_detail(1000000, 13751)
    #end
  #end

  #def test_download_solution
    #[ [22504795] ]
  #end
end
