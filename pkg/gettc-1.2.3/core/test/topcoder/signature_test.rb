require 'test/unit'
require 'topcoder/signature'
include TopCoder

class SignatureTest < Test::Unit::TestCase
    def test_parse_signature
        assert_raise CannotParseSignature do 
            parse_signature 'invalid_signature' 
        end
        assert_raise UnrecognizedType do 
            parse_signature 'strange_type name' 
        end
        assert_raise InvalidVariableName do 
            parse_signature 'int not&ok&name' 
        end
        assert_raise InvalidVariableName do parse_signature 'int 0zero' end
        sig = parse_signature 'String[] a_valid_nam3'
        assert_equal TArray.new(TString), sig.type
        assert_equal 'a_valid_nam3', sig.name
    end
    def test_parse_method_signature
        assert_raise CannotParseSignature do
            parse_method_signature 'there are no brackets'
        end
        assert_raise CannotParseSignature do
            parse_method_signature 'int main(int main())'
        end
        assert_raise CannotParseSignature do
            parse_method_signature 'int main(oops forget to close bracket'
        end
        method  =  '  int   leastBorders(String[] X  , int[] Y, double[] R,'
        method += '  char   my_x1,   long   y1  ,  float x2, int[][] y2) '
        sigs = parse_method_signature method
        assert_equal 8, sigs.size
        assert_equal TInt, sigs[0].type
        assert_equal 'leastBorders', sigs[0].name
        assert_equal TArray.new(TString), sigs[1].type
        assert_equal 'X', sigs[1].name
        assert_equal TArray.new(TInt), sigs[2].type
        assert_equal 'Y', sigs[2].name
        assert_equal TArray.new(TDouble), sigs[3].type
        assert_equal 'R', sigs[3].name
        assert_equal TChar, sigs[4].type
        assert_equal 'my_x1', sigs[4].name
        assert_equal TLong, sigs[5].type
        assert_equal 'y1', sigs[5].name
        assert_equal TFloat, sigs[6].type
        assert_equal 'x2', sigs[6].name
        assert_equal TArray.new(TArray.new(TInt)), sigs[7].type
        assert_equal 'y2', sigs[7].name
    end
end
