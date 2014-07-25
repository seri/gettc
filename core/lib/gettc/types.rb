module Gettc
    TypeError = Class.new StandardError        
    class UnrecognizedType < TypeError
        attr_accessor :type
        def initialize type = nil, msg = "Not a valid TopCoder type"
            @type = type
            super "#{msg} (#{type})"         
        end
    end
    class Type
        def initialize is_object
            @is_object = is_object
        end
        def obj?
            return @is_object
        end
        def to_s
            if self == TInt then
                return "int"
            elsif self == TLong then
                return "long"        
            elsif self == TFloat then
                return "float"
            elsif self == TDouble then
                return "double"
            elsif self == TChar then
                return "char"
            elsif self == TString then
                return "String"
            elsif self == TBoolean then
                return "boolean"
            elsif is_a? TArray then
                return subtype.to_s + "[]"
            end
            return "unknown"
        end
    end
    TBoolean = Type.new false
    TInt = Type.new false
    TLong = Type.new false
    TFloat = Type.new false
    TDouble = Type.new false
    TChar = Type.new false
    TString = Type.new true
    class TArray < Type
        attr_accessor :subtype
        def initialize subtype
            raise UnrecognizedType.new subtype unless subtype.is_a? Type
            @subtype = subtype
        end
        def == ary
            return false unless ary.is_a? TArray
            return @subtype == ary.subtype
        end
        def obj?
            return true
        end
    end
    def parse_type str
        return TArray.new parse_type str[0 .. -3] if str[-2 .. -1] == "[]"
        case str
        when "boolean"
            return TBoolean
        when "int"
            return TInt
        when "long"
            return TLong
        when "float"
            return TFloat
        when "double"
            return TDouble
        when "char"
            return TChar
        when "String"
            return TString
        end
        raise UnrecognizedType.new str
    end
end