require "test/unit"
require "gettc/signature"
require "engine"
include Gettc

class JavascriptEngineTest < Test::Unit::TestCase
  def setup
    func = Signature.new(TArray.new(TDouble), "getMaxMin")
    vars = [
      Signature.new(TArray.new(TArray.new(TInt)), "numbers"),
      Signature.new(TString, "name"),
      Signature.new(TInt, "pivot"),
      Signature.new(TBoolean, "rounded")
    ]
    @engine = JavascriptEngine.new(func, vars)
  end

  def test_arglist
    assert_equal "numbers, name, pivot, rounded", @engine.arglist
  end

  def test_input
    result = <<-eos.gsub(/ {6}/, "")
      var numbers = reader.next('int[][]'); reader.next();
      var name = reader.next('String'); reader.next();
      var pivot = reader.next('int'); reader.next();
      var rounded = reader.next('boolean');
    eos
    assert_equal result.rstrip, @engine.input
  end
end
