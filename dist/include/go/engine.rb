require "gettc/types" 

module Gettc
    class Type
        def to_go
            if self == TInt then
                return "int"
            elsif self == TLong then
                return "int64"        
            elsif self == TFloat then
                return "float32"
            elsif self == TDouble then
                return "float64"
            elsif self == TChar then
                return "byte"
            elsif self == TString then
                return "string"
            elsif self == TBoolean then
                return "bool"
            elsif is_a? TArray then
                return "[]#{subtype.to_go}"
            else
                return "unknown"
            end 
        end
        def dumb_go
            if self == TInt then
                return "0"
            elsif self == TLong then
                return "0"        
            elsif self == TFloat then
                return "0"
            elsif self == TDouble then
                return "0"
            elsif self == TChar then
                return "'$'"
            elsif self == TString then
                return '"$"'
            elsif self == TBoolean then
                return "true"
            elsif is_a? TArray then
                return self.to_go + " {}"
            else
                return "nil"
            end
        end
    end
    class Signature
        def to_go
            return @name + " " + @type.to_go
        end
    end
    class GoEngine
        attr_reader :declare, :input, :output, :func_name
        def initialize func, vars
            @declare = vars.map do |var| 
                var.to_go 
            end
            temp = vars.map do |var| 
                var.name 
            end
            @output = temp.join ", "
            temp = temp.map do |name|
                "&" + name
            end
            @input = temp.join ", "
            @func_name = func.name[0, 1].upcase + func.name[1..-1]
        end
    end
end
