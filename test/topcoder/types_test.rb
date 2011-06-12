require 'test/unit'
require 'topcoder/types'
include TopCoder

class TypesTest < Test::Unit::TestCase
    def test_array_type_error
        assert_raise UnrecognizedType do
            array = TArray.new 123
        end
    end
    def test_parse_type
        assert_raise UnrecognizedType do
            type = parse_type ''
        end
        assert_raise UnrecognizedType do
            type = parse_type ' '
        end
        assert_raise UnrecognizedType do
            type = parse_type '[]'
        end       
        assert_raise UnrecognizedType do
            type = parse_type 'vector'
        end       
        assert_raise UnrecognizedType do
            type = parse_type 'vector[]'
        end       
        assert_equal TInt.instance, parse_type('int')
        assert_equal TLong.instance, parse_type('long')
        assert_equal TFloat.instance, parse_type('float')
        assert_equal TDouble.instance, parse_type('double')
        assert_equal TChar.instance, parse_type('char')
        assert_equal TString.instance, parse_type('String')
        assert_equal TArray.new(TInt.instance), parse_type('int[]')
        assert_not_equal TArray.new(TDouble.instance), parse_type('float[]')
        assert_equal TArray.new(TArray.new(TString.instance)), 
                     parse_type('String[][]')
    end
    def test_to_s
        assert_equal 'int', TInt.instance.to_s
        assert_equal 'vector<long>', TArray.new(TLong.instance).to_s
        assert_equal 'vector<vector<double> >', 
                     TArray.new(TArray.new(TDouble.instance)).to_s
    end
end
