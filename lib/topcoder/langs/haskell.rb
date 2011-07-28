require 'topcoder/types' 

class String
    def uncapitalize
        return self[0, 1].downcase + self[1..-1]
    end
end

module TopCoder
module Langs
    class Haskell
        def self.type_to_s type
            if type.is_a? Types::Int then
                return 'Int'
            elsif type.is_a? Types::Long then
                return 'Integer'        
            elsif type.is_a? Types::Float then
                return 'Float'
            elsif type.is_a? Types::Double then
                return 'Double'
            elsif type.is_a? Types::Char then
                return 'Char'
            elsif type.is_a? Types::String then
                return 'String'
            elsif type.is_a? Types::Array then
                return '[' + self.type_to_s(type.subtype) + ']'
            else
                return 'Unknown'
            end 
        end
        def self.parser type
            if type.is_a? Types::Int then
                return 'parseInt'
            elsif type.is_a? Types::Long then
                return 'parseLong'        
            elsif type.is_a? Types::Float then
                return 'parseFloat'
            elsif type.is_a? Types::Double then
                return 'parseDouble'
            elsif type.is_a? Types::Char then
                return 'parseChar'
            elsif type.is_a? Types::String then
                return 'parseString'
            elsif type.is_a? Types::Array then
                return '(parseList ' + self.parser(type.subtype) + ')'
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
                return '[]'
            else
                return 'Nil'
            end
        end
        attr_accessor :func, :vars, :var_types, :var_names
        def initialize func, vars
            @func = func
            @vars = vars            
            @var_types = @vars.map do |var| 
                self.class.type_to_s var.type
            end
            @var_names = @vars.map do |var|
                var.name.uncapitalize
            end
        end
        def input
            ret = 'getVars = do '
            indent = ' ' * ret.size
            temp = @vars.map do |var|
                x = var.name.uncapitalize
                x << ' <- spaces >> ' 
                x << self.class.parser(var.type)
            end
            ret << temp.join(" ; next\n#{indent}")
            ret << "\n"
            ret << indent << 'return (' << var_names.join(', ') << ')'
            return ret
        end
    end
end
end
