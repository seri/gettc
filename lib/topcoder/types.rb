require 'singleton'
    
module TopCoder 
    class TypeError < StandardError        
    end
    class UnrecognizedType < TypeError
        attr_accessor :type
        def initialize type = nil, msg = 'Not a valid TopCoder type'
            @type = type
            super "#{msg} (#{type})"         
        end
    end
module Types
    class Type
        def obj?
            return false
        end
    end
    class Int < Type
        include Singleton
    end
    class Long < Type
        include Singleton
    end
    class Float < Type
        include Singleton
    end
    class Double < Type
        include Singleton
    end
    class Char < Type
        include Singleton
    end
    class String < Type
        include Singleton
        def obj?
            return true
        end
    end
    class Array < Type
        attr_accessor :subtype
        def initialize subtype
            raise UnrecognizedType.new subtype if not subtype.is_a? Type
            @subtype = subtype
        end
        def == ary
            return false if not ary.is_a? Array
            return @subtype == ary.subtype
        end
        def obj?
            return true
        end
    end
end
    def parse_type str
        return Types::Array.new parse_type str[0 .. -3] if str[-2 .. -1] == '[]'
        case str
            when 'int'
                return Types::Int.instance
            when 'long'
                return Types::Long.instance
            when 'float'
                return Types::Float.instance
            when 'double'
                return Types::Double.instance
            when 'char'
                return Types::Char.instance
            when 'String'
                return Types::String.instance
        end
        raise UnrecognizedType.new str
    end
end
