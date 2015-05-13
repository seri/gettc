require "gettc/types"

module Gettc
    class Type
        def to_cpp
            if is_a? TArray then
                ret = "vector<" << subtype.to_cpp
                ret << " " if subtype.is_a? TArray
                ret << ">"
                return ret
            end

            case self
            when TInt
                return "int"
            when TLong
                return "int64"        
            when TFloat
                return "float"
            when TDouble
                return "double"
            when TChar
                return "char"
            when TString
                return "string"
            when TBoolean
                return "bool"
            end

            return "unknown"
        end
        def dumb_cpp
            if is_a? TArray then
                return "#{to_cpp}()"
            end

            case self
            when TInt, TLong, TFloat, TDouble
                return "0"
            when TChar
                return "'$'"
            when TString
                return '"$"'
            when TBoolean then
                return "true"
            end

            return "Nil"
        end
    end
    class Signature
        def to_cpp declaring = false
            ret = type.to_cpp
            ret << " "
            ret << "const &" if declaring and type.obj?
            ret << name 
            return ret
        end
    end
    class CppEngine
        attr_reader :declare, :input, :output
        def initialize func, vars
            @func = func
            @vars = vars            
            compute_declare
            compute_input
            compute_output
        end
    private
        def compute_declare
            @declare = @func.to_cpp
            @declare << "("
                indent = " " * @declare.size
                temp = @vars.map do |var| 
                    var.to_cpp true 
                end
                @declare << temp.join(",\n#{indent}")
            @declare << ")"
        end
        def compute_input
            temp = @vars.map do |var|
                x = var.to_cpp
                x << "; tc::read(ifs, "
                x << var.name
                x << ");"
            end
            @input = temp.join " tc::next(ifs);\n"
        end
        def compute_output
            @output = "tc::show(ofs, solver." << @func.name << "("
            temp = @vars.map do |var| 
                var.name 
            end
            @output << temp.join(", ") << "));"
        end
    end
end
