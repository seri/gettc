require "gettc/types" 

module Gettc
    class Type
        def dumb_python
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
                return "True"
            elsif self.is_a? TArray then
                return "[]"
            else
                return "None"
            end
        end
    end
    class PythonEngine
        attr_accessor :func, :vars
        def initialize func, vars
            @func = func
            @vars = vars            
        end
        def vars_list
            temp = @vars.map do |var|
                var.name
            end
            return temp.join ", "
        end
        def input
            temp = @vars.map do |var| 
                var.name + ' = reader.next("' + var.type.to_s + '")'
            end
            return temp.join "\nreader.next()\n"
        end
    end
end