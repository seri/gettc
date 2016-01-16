require "test/unit"
require "gettc/signature" 
require "engine" 
include Gettc

class RubyEngineTest < Test::Unit::TestCase
  def setup
    func = Signature.new TArray.new(TDouble), "getMaxMin"
    vars = [ Signature.new(TArray.new(TArray.new(TInt)), "numbers"),
         Signature.new(TString, "name"),
         Signature.new(TInt, "pivot"),
         Signature.new(TBoolean, "rounded") ]
    @engine = RubyEngine.new func, vars
  end
  def test_arglist
    assert_equal "numbers, name, pivot, rounded", @engine.arglist
  end
  def test_input
    result = <<-eos.gsub(/ {12}/, "")
      numbers = reader.next(TArray.new(TArray.new(TInt)))
      reader.next()
      name = reader.next(TString)
      reader.next()
      pivot = reader.next(TInt)
      reader.next()
      rounded = reader.next(TBoolean)
    eos
    assert_equal result.rstrip, @engine.input
  end
end
