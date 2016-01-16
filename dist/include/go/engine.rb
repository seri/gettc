require "gettc/types"

module Gettc
  class Type
    def to_go
      return "[]#{subtype.to_go}" if self.is_a?(TArray)

      case self
      when TInt
        "int"
      when TLong
        "int64"
      when TFloat
        "float32"
      when TDouble
        "float64"
      when TChar
        "byte"
      when TString
        "string"
      when TBoolean
        "bool"
      else
        "unknown"
      end
    end

    def dumb_go
      return "#{self.to_go} {}" if self.is_a?(TArray)

      case self
      when TInt, TLong, TDouble, TFloat
        "0"
      when TChar
        "'$'"
      when TString
        '"$"'
      when TBoolean
        "true"
      else
        "nil"
      end
    end
  end

  class Signature
    def to_go
      "#{@name} #{@type.to_go}"
    end
  end

  class GoEngine
    attr_reader :declare, :input, :output, :func_name

    def initialize(func, vars)
      @declare = vars.map(&:to_go)
      @output = vars.map(&:name).join(", ")
      @input = vars.map { |var| "&#{var.name}" }.join(", ")
      @func_name = func.name[0, 1].upcase + func.name[1..-1]
    end
  end
end
