require 'test/unit'
require 'topcoder/langs/cpp' 
include TopCoder
include TopCoder::Langs

class CppTest < Test::Unit::TestCase
    def test_type_to_s
        assert_equal 'int', 
                     Cpp::type_to_s(Types::Int.instance)
        assert_equal 'vector<long>', 
                     Cpp::type_to_s(Types::Array.new(Types::Long.instance))
        assert_equal 'vector<vector<double> >', 
         Cpp::type_to_s(Types::Array.new(Types::Array.new(Types::Double.instance)))
        assert_equal 'double ret = 0;',
                     Cpp::intro(Types::Double.instance, 'ret')
        assert_equal 'vector<string> ret;',
                     Cpp::intro(Types::Array.new(Types::String.instance), 'ret')
    end
end
