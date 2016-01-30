require "test/unit"

require "gettc/types"
include Gettc

require "topcoder/writer"
require "topcoder/reader"
include TopCoder

class WriterTest < Test::Unit::TestCase
  def check(value, type)
    assert_equal Reader.new(Writer.new.next(value, type).to_s).next(type), value
  end

  def test_write_single_value
    check 20.14, TDouble
    check '@', TChar
    check 'String white a "', TString
    check [1, 2, 3, 4], TArray.new(TInt)
    check [[20.14]], TArray.new(TArray.new(TDouble))
    check [[[]]], TArray.new(TArray.new(TArray.new(TBoolean)))
    check false, TBoolean
    check [["Jon Snow"],['The "Little Finger"']], TArray.new(TArray.new(TString))

    assert_raise UnsupportedType do
      Writer.new.next(Hash.new, "dict")
    end
  end

  def test_write_all
    writer = Writer.new
    writer.next("Seri", TString)
    writer.next
    writer.next(20.14, TFloat)
    writer.next
    writer.next([1, 2, 3, 4], TArray.new(TInt))
    assert_equal '"Seri", 20.14, [1, 2, 3, 4]', writer.to_s
  end
end
