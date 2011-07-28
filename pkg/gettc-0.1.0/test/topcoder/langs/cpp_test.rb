require 'test/unit'
require 'topcoder/langs/cpp' 
include TopCoder
include TopCoder::Langs

class CppTest < Test::Unit::TestCase
    def test_type_to_s
        assert_equal 'int', 
                     Cpp::type_to_s(Types::Int.instance)
        assert_equal 'vector<int64>', 
                     Cpp::type_to_s(Types::Array.new(Types::Long.instance))
        assert_equal 'vector<vector<double> >', 
         Cpp::type_to_s(Types::Array.new(Types::Array.new(Types::Double.instance)))
    end
end
