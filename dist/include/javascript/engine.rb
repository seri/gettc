require "gettc/types"

module Gettc
    class Type
        def dumb_javascript
            if is_a? TArray then
                return "[]"
            end

            case self
            when TInt, TLong, TFloat, TDouble
                return "0"
            when TChar
                return "'$'"
            when TString
                return '"$"'
            when TBoolean
                return "true"
            end

            return "null"
        end
    end
    class JavascriptEngine
        attr_reader :arglist, :input
        def initialize func, vars
            temp = vars.map do |var|
                var.name
            end
            @arglist = temp.join ", "

            temp = vars.map do |var| 
                "var " + var.name + ' = reader.next("' + var.type.to_s + '");'
            end
            @input = temp.join " reader.next();\n"
        end
    end
end
