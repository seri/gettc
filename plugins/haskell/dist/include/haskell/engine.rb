require "gettc/types" 

module Gettc
    class Type
        def to_haskell
            if is_a? TArray then
                return "[" + subtype.to_haskell + "]"
            end

            case self
            when TInt
                return "Int"
            when TLong
                return "Integer"        
            when TFloat
                return "Float"
            when TDouble
                return "Double"
            when TChar
                return "Char"
            when TString
                return "String"
            when TBoolean
                return "Bool"
            end 

            return "Unknown"
        end
        def get_haskell_parser
            if is_a? TArray then
                return "(TC.parseList " + subtype.get_haskell_parser + ")"
            end
            
            case self
            when TInt
                return "TC.parseInt"
            when TLong
                return "TC.parseLong"        
            when TFloat
                return "TC.parseFloat"
            when TDouble
                return "TC.parseDouble"
            when TChar
                return "TC.parseChar"
            when TString
                return "TC.parseString"
            when TBoolean
                return "TC.parseBool"
            end

            return "unknown"
        end
        def dumb_haskell
            if is_a? TArray
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
                return "True"
            end

            return "Nil"
        end
    end
    class HaskellEngine
        attr_reader :func, :vars, :declare, :input, :output
        def initialize func, vars
            @func = Signature.new func.type, uncapitalize(func.name)
            @vars = vars.map do |var|
                Signature.new var.type, uncapitalize(var.name)
            end
            compute_declare
            compute_input
            compute_output
        end
    private
        def uncapitalize str
            return str[0, 1].downcase + str[1..-1]
        end
        def compute_declare
            ret = ""
            ret << func.name << " :: "

            temp = vars.map do |var| var.type.to_haskell end
            ret << temp.join(" -> ") << " -> " << func.type.to_haskell << "\n"

            ret << func.name << " "
            temp = vars.map do |var| var.name end
            ret << temp.join(" ") << " = " << func.type.dumb_haskell

            @declare = ret
        end
        def compute_input
            ret = "getVars :: TC.Parser ("
            temp = @vars.map do |var| var.type.to_haskell end
            ret << temp.join(", ") << ")\n"
            temp = "getVars = do "
            ret << temp
                indent = " " * temp.size
                temp = @vars.map do |var|
                    x = ""
                    x << var.name
                    x << " <- TC.spaces >> " 
                    x << var.type.get_haskell_parser
                end
            ret << temp.join(" ; TC.spaces >> TC.next\n#{indent}") << "\n"
            ret << indent << "return ("
                temp = @vars.map do |var| var.name end
                ret << temp.join(", ")
            ret << ")"
            @input = ret
        end
        def compute_output
            ret = ""
            ret << @func.name << " "
            temp = vars.map do |var| var.name end
            ret << temp.join(" ")
            @output = ret
        end
    end
end
