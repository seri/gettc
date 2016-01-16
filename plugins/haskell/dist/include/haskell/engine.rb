require "gettc/types"

module Gettc
  class Type
    def to_haskell
      return "[#{subtype.to_haskell}]" if self.is_a?(TArray)

      case self
      when TInt
        "Int"
      when TLong
        "Integer"
      when TFloat
        "Float"
      when TDouble
        "Double"
      when TChar
        "Char"
      when TString
        "String"
      when TBoolean
        "Bool"
      else
        "Unknown"
      end
    end

    def get_haskell_parser
      return "(TC.parseList #{subtype.get_haskell_parser})" if self.is_a?(TArray)

      case self
      when TInt
        "TC.parseInt"
      when TLong
        "TC.parseLong"
      when TFloat
        "TC.parseFloat"
      when TDouble
        "TC.parseDouble"
      when TChar
        "TC.parseChar"
      when TString
        "TC.parseString"
      when TBoolean
        "TC.parseBool"
      else
        "unknown"
      end
    end

    def dumb_haskell
      return "[]" if self.is_a?(TArray)

      case self
      when TInt, TLong, TFloat, TDouble
        "0"
      when TChar
        "'$'"
      when TString
        '"$"'
      when TBoolean
        "True"
      else
        "Nil"
      end
    end
  end

  class HaskellEngine
    attr_reader :func, :vars

    def initialize(func, vars)
      @func = Signature.new(func.type, uncapitalize(func.name))
      @vars = vars.map { |var| Signature.new(var.type, uncapitalize(var.name)) }
    end

    def declare
      ret = ""
      ret << @func.name << " :: "
      ret << @vars.map { |var| var.type.to_haskell }.join(" -> ")
      ret << " -> " << @func.type.to_haskell << "\n"
      ret << @func.name << " "
      ret << @vars.map(&:name).join(" ")
      ret << " = " << @func.type.dumb_haskell
    end

    def input
      ret = "getVars :: TC.Parser ("
      ret << @vars.map { |var| var.type.to_haskell }.join(", ") << ")\n"

      temp = "getVars = do "
      indent = " " * temp.size
      ret << temp

      ret << @vars.map do |var|
        x = ""
        x << var.name
        x << " <- TC.spaces >> "
        x << var.type.get_haskell_parser
      end.join(" ; TC.spaces >> TC.next\n#{indent}")

      ret << "\n"
      ret << indent << "return ("
      ret << @vars.map(&:name).join(", ")
      ret << ")"
    end

    def output
      @func.name + " " + @vars.map(&:name).join(" ")
    end

    private

    def uncapitalize str
      str[0, 1].downcase + str[1..-1]
    end
  end
end
