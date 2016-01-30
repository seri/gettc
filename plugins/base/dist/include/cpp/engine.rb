require "gettc/types"

module Gettc
  class Type
    def to_cpp
      if self.is_a?(TArray)
        ret = "vector<" << subtype.to_cpp
        ret << " " if subtype.is_a?(TArray)
        ret << ">"
        return ret
      end

      case self
      when TInt
        "int"
      when TLong
        "int64"
      when TFloat
        "float"
      when TDouble
        "double"
      when TChar
        "char"
      when TString
        "string"
      when TBoolean
        "bool"
      else
        "unknown"
      end
    end

    def dumb_cpp
      return "#{to_cpp}()" if self.is_a?(TArray)

      case self
      when TInt, TLong, TFloat, TDouble
        "0"
      when TChar
        "'$'"
      when TString
        '"$"'
      when TBoolean
        "true"
      else
        "Nil"
      end
    end
  end

  class Signature
    def to_cpp(declaring = false)
      ret = type.to_cpp
      ret << " "
      ret << "const &" if declaring && type.obj?
      ret << name
      ret
    end
  end

  class CppEngine
    def initialize(func, vars)
      @func = func
      @vars = vars
    end

    def declare
      ret = @func.to_cpp
      ret << "("
      indent = " " * ret.size
      ret << @vars.map { |var| var.to_cpp(true) }.join(",\n#{indent}")
      ret << ")"
    end

    def input
      @vars.map do |var|
        x = var.to_cpp
        x << "; tc::read(ifs, "
        x << var.name
        x << ");"
      end.join(" tc::next(ifs);\n")
    end

    def output
      ret = "tc::show(ofs, solver." << @func.name << "("
      ret << @vars.map(&:name).join(", ") << "));"
    end
  end
end
