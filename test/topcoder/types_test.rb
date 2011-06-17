require 'test/unit'
require 'topcoder/types'
include TopCoder

class TypesTest < Test::Unit::TestCase
    def test_array_type_error
        assert_raise UnrecognizedType do
            array = Types::Array.new 123
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
        assert_equal Types::Int.instance, parse_type('int')
        assert_equal Types::Long.instance, parse_type('long')
        assert_equal Types::Float.instance, parse_type('float')
        assert_equal Types::Double.instance, parse_type('double')
        assert_equal Types::Char.instance, parse_type('char')
        assert_equal Types::String.instance, parse_type('String')
        assert_equal Types::Array.new(Types::Int.instance), parse_type('int[]')
        assert_not_equal Types::Array.new(Types::Double.instance), 
                         parse_type('float[]')
        assert_equal Types::Array.new(Types::Array.new(Types::String.instance)), 
                     parse_type('String[][]')
    end
end
