require 'test/unit'
require 'topcoder/types'
include TopCoder

class TypesTest < Test::Unit::TestCase
    def test_parse_type
        assert_raise UnrecognizedType do array = TArray.new 123 end
        assert_raise UnrecognizedType do type = parse_type '' end
        assert_raise UnrecognizedType do type = parse_type ' ' end
        assert_raise UnrecognizedType do type = parse_type '[]' end       
        assert_raise UnrecognizedType do type = parse_type 'vector' end       
        assert_raise UnrecognizedType do type = parse_type 'vector[]' end       
        assert_equal TBoolean, parse_type('boolean')
        assert_equal TInt, parse_type('int')
        assert_equal TLong, parse_type('long')
        assert_equal TFloat, parse_type('float')
        assert_equal TDouble, parse_type('double')
        assert_equal TChar, parse_type('char')
        assert_equal TString, parse_type('String')
        assert_equal TArray.new(TBoolean), parse_type('boolean[]')
        assert_equal TArray.new(TInt), parse_type('int[]')
        assert_equal TArray.new(TArray.new(TString)), parse_type('String[][]')
    end
end
