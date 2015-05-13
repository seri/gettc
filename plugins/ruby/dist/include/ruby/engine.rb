require "gettc/types"

module Gettc
    class Type
        def to_ruby
            if is_a? TArray
                return "TArray.new(#{subtype.to_ruby})"
            end 

            case self
            when TInt
                return "TInt"
            when TLong
                return "TLong"
            when TFloat
                return "TFloat"
            when TDouble
                return "TDouble"
            when TChar
                return "TChar"
            when TString
                return "TString"
            when TBoolean
                return "TBoolean"
            end

            return "Object"
        end
        def dumb_ruby
            if is_a? TArray then
                return "[]"
            end

            case self
            when TInt, TLong, TFloat, TDouble
                return "0"
            when TChar
                return "?$"
            when TString
                return '"$"'
            when TBoolean
                return "true"
            end

            return "nil"
        end
    end
    class RubyEngine
        attr_reader :arglist, :input
        def initialize func, vars
            temp = vars.map do |var|
                var.name
            end
            @arglist = temp.join ", "

            temp = vars.map do |var| 
                var.name + " = reader.next(" + var.type.to_ruby + ")"
            end
            @input = temp.join "\nreader.next()\n"
        end
    end
end
