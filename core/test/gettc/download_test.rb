require "test/unit"
require "gettc/download"
include Gettc

class DownloadTest < Test::Unit::TestCase
  def setup
    @account = Account.new("gettc", "algorithm")
  end

  def test_wrong_account
    assert_raises LoginFailed do
      Downloader.new(Account.new("username", "password")).download_problem(11127)
    end
  end

  def test_wrong_id
    assert_raises IDNotAvailable do Downloader.new(@account).download_problem(1000000) end
  end

  def test_download_ok
    downloader = Downloader.new(@account)
    [
      10297, 10324, 10329, 10330, 10505, 10685, 10686, 10690, 11264,
      11266, 11303, 11315, 11322, 11350, 11357, 11419, 8763, 8819, 9995
    ].shuffle.take(3).each do |id|
      assert_match "<h3>Problem Statement</h3>", downloader.download_problem(id)
    end
  end
end
