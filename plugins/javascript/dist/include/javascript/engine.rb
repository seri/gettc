require "gettc/types"

module Gettc
  class Type
    def dumb_javascript
      return "[]" if self.is_a?(TArray)

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
        "null"
      end
    end
  end

  class JavascriptEngine
    attr_reader :arglist, :input

    def initialize func, vars
      @arglist = vars.map(&:name).join(", ")

      @input = vars.map do |var|
        "var #{var.name} = reader.next('#{var.type}');"
      end.join(" reader.next();\n")
    end
  end
end
