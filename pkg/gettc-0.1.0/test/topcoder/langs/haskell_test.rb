require 'test/unit'
require 'topcoder/langs/haskell' 
include TopCoder
include TopCoder::Langs

class HaskellTest < Test::Unit::TestCase
    def test_to_hs
        assert_equal 'Int', 
                     Haskell::type_to_s(Types::Int.instance)
        assert_equal '[Long]', 
                     Haskell::type_to_s(Types::Array.new(Types::Long.instance))
        assert_equal '[[Double]]', 
         Haskell::type_to_s(Types::Array.new(Types::Array.new(Types::Double.instance)))
    end
end
