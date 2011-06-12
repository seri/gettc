require 'singleton'
    
module TopCoder 
    class TTypeError < StandardError        
    end
    class UnrecognizedType < TTypeError
        attr_accessor :type
        def initialize type = nil, msg = 'Not a valid TopCoder type'
            @type = type
            super "#{msg} (#{type})"         
        end
    end
    class Type
        def to_cpp
            return ''
        end
        def to_hs
            return ''
        end
        def to_s
            return to_cpp
        end
        def dumb_cpp
            return 'nil'
        end
        def dumb_hs
            return 'nil'
        end
        def obj?
            return false
        end
    end
    class TInt < Type
        include Singleton
        def to_cpp
            return 'int'
        end
        def to_hs
            return 'Int'
        end
        def dumb_cpp
            return '0'
        end
        def dumb_hs
            return '0'
        end
    end
    class TLong < Type
        include Singleton
        def to_cpp
            return 'long'
        end
        def to_hs
            return 'Int'
        end
        def dumb_cpp
            return '0'
        end
        def dumb_hs
            return '0'
        end
    end
    class TFloat < Type
        include Singleton
        def to_cpp
            return 'float'
        end
        def to_hs
            return 'Float'
        end
        def dumb_cpp
            return '0'
        end
        def dumb_hs
            return '0'
        end
    end
    class TDouble < Type
        include Singleton
        def to_cpp
            return 'double'
        end
        def to_hs
            return 'Double'
        end
        def dumb_cpp
            return '0'
        end
        def dumb_hs
            return '0'
        end
    end
    class TChar < Type
        include Singleton
        def to_cpp
            return 'char'
        end
        def to_hs
            return 'Char'
        end
        def dumb_cpp
            return "'$'"
        end
        def dumb_hs
            return "'$'"
        end
    end
    class TString < Type
        include Singleton
        def to_cpp
            return 'string'
        end
        def to_hs
            return 'String'
        end
        def dumb_cpp
            return '"$"'            
        end
        def dumb_hs
            return '"$"'            
        end
        def obj?
            return true
        end
    end
    class TArray < Type
        attr_accessor :subtype
        def initialize subtype
            if not subtype.is_a? Type then
                raise UnrecognizedType.new subtype
            end 
            @subtype = subtype
        end
        def to_cpp
            return "vector<#{subtype}>"
        end
        def to_hs
            return "[#{subtype.to_hs}]"
        end
        def dumb_cpp
            return "#{to_cpp}()"
        end
        def dumb_hs
            return '[]'
        end
        def == ary
            return false if not ary.is_a? TArray
            return @subtype == ary.subtype
        end
        def obj?
            return true
        end
    end
    def parse_type str
        return TArray.new parse_type(str[0 .. -3]) if str[-2 .. -1] == '[]'
        case str
            when 'int'
                return TInt.instance
            when 'long'
                return TLong.instance
            when 'float'
                return TFloat.instance
            when 'double'
                return TDouble.instance
            when 'char'
                return TChar.instance
            when 'String'
                return TString.instance
        end
        raise UnrecognizedType.new str
    end
end
