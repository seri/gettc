require "test/unit" 

require "gettc/types"
include Gettc

require "topcoder/reader"
include TopCoder

class ReaderTest < Test::Unit::TestCase
    def test_unsupported_type
        reader = Reader.new "whatever"
        assert_raise UnsupportedType do
            reader.next "object"
        end
    end

    def test_read_boolean
        reader = Reader.new "true"
        flag = reader.next TBoolean
        assert_equal true, flag

        reader = Reader.new "false"
        flag = reader.next TBoolean
        assert_equal false, flag
    end
    def test_read_boolean_error
        reader = Reader.new "lol"
        assert_raise ParseError do
            flag = reader.next TBoolean
        end
    end

    def test_read_positive_int
        reader = Reader.new "20.14"
        number = reader.next TInt
        assert_equal 20, number
    end
    def test_read_negative_int
        reader = Reader.new "-1"
        number = reader.next TInt
        assert_equal -1, number
    end
    def test_read_int_error
        reader = Reader.new "NaN"
        assert_raise ParseError do
            number = reader.next TInt
        end
    end
    def test_read_long
        str = "1234567890123456789"
        reader = Reader.new str
        number = reader.next TLong
        assert_equal str, number.to_s
    end

    def test_read_positive_float
        reader = Reader.new "20.14.123"
        number = reader.next TFloat
        assert_equal 20.14, number
    end
    def test_read_negative_float
        reader = Reader.new "-6210"
        number = reader.next TFloat
        assert_equal -6210, number
    end


    def test_read_quoted_char
        reader = Reader.new "'@'"
        character = reader.next TChar
        assert_equal '@', character
    end
    def test_read_unquoted_char
        reader = Reader.new "@"
        character = reader.next TChar
        assert_equal '@', character
    end
    def test_read_char_error
        reader = Reader.new "'@ '"
        assert_raise ParseError do
            number = reader.next TChar
        end
    end


    def test_read_string
        reader = Reader.new '"Eddard Stark"'
        name = reader.next TString
        assert_equal "Eddard Stark", name
    end
    def test_read_string_error
        reader = Reader.new '"Eddard Stark'
        assert_raise ParseError do
            name = reader.next TString
        end
        reader = Reader.new 'Eddard Stark"'
        assert_raise ParseError do
            name = reader.next TString
        end
    end
    def test_read_string_with_quote
        reader = Reader.new '"Eddard "Ned"", "Stark"'
        name = reader.next TString
        assert_equal 'Eddard "Ned"', name
    end

    def test_read_empty_array
        reader = Reader.new "[]"
        arr = reader.next TArray.new(TInt)
        assert arr.empty?
    end
    def test_read_array
        reader = Reader.new "[2, 3, 1, 4, 6, 5]"
        arr = reader.next TArray.new(TInt)
        assert_equal [2, 3, 1, 4, 6, 5], arr
    end
    def test_read_array_2d
        reader = Reader.new '[ ["abc", "def", "h"], ["foo", "bar"] ]'
        arr = reader.next TArray.new(TArray.new(TString))
        assert_equal [["abc", "def", "h"], ["foo", "bar"]], arr
    end
    def test_read_array_error
        reader = Reader.new "[1, 2, 3"
        assert_raise ParseError do
            arr = reader.next TArray.new(TInt)
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
        reader = Reader.new text
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
