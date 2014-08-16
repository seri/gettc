require "gettc/types" 

module Gettc
    class Type
        def to_haskell
            if self == TInt then
                return "Int"
            elsif self == TLong then
                return "Integer"        
            elsif self == TFloat then
                return "Float"
            elsif self == TDouble then
                return "Double"
            elsif self == TChar then
                return "Char"
            elsif self == TString then
                return "String"
            elsif self == TBoolean then
                return "Bool"
            elsif is_a? TArray then
                return "[" + subtype.to_haskell + "]"
            else
                return "Unknown"
            end 
        end
        def get_haskell_parser
            if self == TInt then
                return "TC.parseInt"
            elsif self == TLong then
                return "TC.parseLong"        
            elsif self == TFloat then
                return "TC.parseFloat"
            elsif self == TDouble then
                return "TC.parseDouble"
            elsif self == TChar then
                return "TC.parseChar"
            elsif self == TString then
                return "TC.parseString"
            elsif self == TBoolean then
                return "TC.parseBool"
            elsif is_a? TArray then
                return "(TC.parseList " + subtype.get_haskell_parser + ")"
            else
                return "unknown"
            end
        end
        def dumb_haskell
            if self == TInt then
                return "0"
            elsif self == TLong then
                return "0"        
            elsif self == TFloat then
                return "0"
            elsif self == TDouble then
                return "0"
            elsif self == TChar then
                return "\"$\""
            elsif self == TString then
                return "\"$\""
            elsif self == TBoolean then
                return "True"
            elsif is_a? TArray then
                return "[]"
            else
                return "Nil"
            end
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
