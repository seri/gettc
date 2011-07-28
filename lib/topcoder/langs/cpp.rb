require 'topcoder/types' 

module TopCoder
module Langs
    class Cpp
        def self.type_to_s type
            if type.is_a? Types::Int then
                return 'int'
            elsif type.is_a? Types::Long then
                return 'int64'        
            elsif type.is_a? Types::Float then
                return 'float'
            elsif type.is_a? Types::Double then
                return 'double'
            elsif type.is_a? Types::Char then
                return 'char'
            elsif type.is_a? Types::String then
                return 'string'
            elsif type.is_a? Types::Array then
                ret = "vector<" << self.type_to_s(type.subtype)
                if type.subtype.is_a? Types::Array then
                    ret << " "
                end 
                ret << ">"
                return ret
            else
                return 'unknown'
            end 
        end
        def self.dumb_value type
            if type.is_a? Types::Int then
                return '0'
            elsif type.is_a? Types::Long then
                return '0'        
            elsif type.is_a? Types::Float then
                return '0'
            elsif type.is_a? Types::Double then
                return '0'
            elsif type.is_a? Types::Char then
                return "'$'"
            elsif type.is_a? Types::String then
                return '"$"'
            elsif type.is_a? Types::Array then
                return "#{self.type_to_s type}()"
            else
                return 'Nil'
            end
        end
        def self.sig_to_s sig, declaring = false
            type, name = sig.type, sig.name
            ret = self.type_to_s type
            ret << ' '
            if declaring and type.obj? then
                ret << 'const &'
            end 
            ret << name 
            return ret
        end
        attr_accessor :func, :vars, :var_types, :var_names
        def initialize func, vars
            @func = func
            @vars = vars            
            @var_types = @vars.map do |var| 
                self.class.sig_to_s var, true
            end
            @var_names = @vars.map do |var|
                var.name
            end 
        end
        def declare
            ret = self.class.sig_to_s @func
            ret << '('
                indent = ' ' * ret.size
                ret << @var_types.join(",\n#{indent}")
            ret << ')'
            return ret
        end
        def input
            indent = ' ' * 8
            ret = indent
            temp = @vars.map do |var|
                x = self.class.sig_to_s(var)
                x << '; read(ifs, '
                x << var.name
                x << ');'
            end
            ret << temp.join(" next(ifs);\n#{indent}")
            return ret
        end
    end
end
end
