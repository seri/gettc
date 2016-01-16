require "test/unit"

require "gettc/types"
include Gettc

require "topcoder/reader"
include TopCoder

class ReaderTest < Test::Unit::TestCase
  def test_unsupported_type
    assert_raise UnsupportedType do
      Reader.new("whatever").next("object")
    end
  end

  def test_read_boolean
    assert_equal true, Reader.new("true").next(TBoolean)
    assert_equal false, Reader.new("false").next(TBoolean)
  end

  def test_read_boolean_error
    assert_raise ParseError do
      Reader.new("lol").next(TBoolean)
    end
  end

  def test_read_positive_int
    assert_equal 20, Reader.new("20.14").next(TInt)
  end

  def test_read_negative_int
    assert_equal -1, Reader.new("-1").next(TInt)
  end

  def test_read_int_error
    assert_raise ParseError do
      Reader.new("NaN").next(TInt)
    end
  end

  def test_read_long
    str = "1234567890123456789"
    assert_equal str, Reader.new(str).next(TLong).to_s
  end

  def test_read_positive_float
    assert_equal 20.14, Reader.new("20.14.123").next(TFloat)
  end

  def test_read_negative_float
    assert_equal -6210, Reader.new("-6210").next(TFloat)
  end

  def test_read_quoted_char
    assert_equal '@', Reader.new("'@'").next(TChar)
  end

  def test_read_unquoted_char
    assert_equal '@', Reader.new("@").next(TChar)
  end

  def test_read_char_error
    assert_raise ParseError do
      Reader.new("'@ '").next(TChar)
    end
  end

  def test_read_string
    assert_equal "Eddard Stark", Reader.new('"Eddard Stark"').next(TString)
  end

  def test_read_string_error
    assert_raise ParseError do
      Reader.new('"Eddard Stark').next(TString)
    end

    assert_raise ParseError do
      Reader.new('Eddard Stark"').next(TString)
    end
  end

  def test_read_string_with_quote
    assert_equal 'Eddard "Ned"', Reader.new('"Eddard "Ned"", "Stark"').next(TString)
  end

  def test_read_empty_array
    assert Reader.new("[]").next(TArray.new(TInt)).empty?
  end

  def test_read_array
    assert_equal [2, 3, 1, 4, 6, 5], Reader.new("[2, 3, 1, 4, 6, 5]").next(TArray.new(TInt))
  end

  def test_read_array_2d
    assert_equal [["abc", "def", "h"], ["foo", "bar"]],
      Reader.new('[ ["abc", "def", "h"], ["foo", "bar"] ]').next(TArray.new(TArray.new(TString)))
  end

  def test_read_array_error
    assert_raise ParseError do
      Reader.new("[1, 2, 3").next(TArray.new(TInt))
    end
  end

  def test_read_everything
    text = <<-eos
      [ [ "Jon Snow"
        , "Lord Varys"
        , "The "Little Finger"" ]
      , [ ] ]
      , C, 20.14, fAlSe, 'x'
      , [ -2 , 0 , 1 , 4 ]
    eos
    reader = Reader.new(text)

    assert_equal [["Jon Snow", "Lord Varys", 'The "Little Finger"'], []], reader.next(TArray.new(TArray.new(TString)))
    reader.next

    assert_equal 'C', reader.next(TChar)
    reader.next

    assert_equal 20.14, reader.next(TDouble)
    reader.next

    assert_equal false, reader.next(TBoolean)
    reader.next

    assert_equal 'x', reader.next(TChar)
    reader.next

    assert_equal [-2, 0, 1, 4], reader.next(TArray.new(TInt))
  end
end
