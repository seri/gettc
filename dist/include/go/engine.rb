require "gettc/types" 

module Gettc
    class Type
        def to_go
            if is_a? TArray
                return "[]#{subtype.to_go}"
            end

            case self 
            when TInt
                return "int"
            when TLong
                return "int64"        
            when TFloat
                return "float32"
            when TDouble
                return "float64"
            when TChar
                return "byte"
            when TString
                return "string"
            when TBoolean
                return "bool"
            end 

            return "unknown"
        end
        def dumb_go
            if is_a? TArray
                return to_go + " {}"
            end

            case self
            when TInt, TLong, TDouble, TFloat
                return "0"
            when TChar
                return "'$'"
            when TString
                return '"$"'
            when TBoolean
                return "true"
            end

            return "nil"
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
