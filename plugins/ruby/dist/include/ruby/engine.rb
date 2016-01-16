require "gettc/types"

module Gettc
  class Type
    def to_ruby
      return "TArray.new(#{subtype.to_ruby})" if self.is_a?(TArray)

      case self
      when TInt
        "TInt"
      when TLong
        "TLong"
      when TFloat
        "TFloat"
      when TDouble
        "TDouble"
      when TChar
        "TChar"
      when TString
        "TString"
      when TBoolean
        "TBoolean"
      else
        "Object"
      end
    end

    def dumb_ruby
      return "[]" if self.is_a?(TArray)

      case self
      when TInt, TLong, TFloat, TDouble
        "0"
      when TChar
        "?$"
      when TString
        '"$"'
      when TBoolean
        "true"
      else
        "nil"
      end
    end
  end

  class RubyEngine
    attr_reader :arglist, :input

    def initialize(func, vars)
      @arglist = vars.map(&:name).join(", ")

      @input = vars.map do |var|
        "#{var.name} = reader.next(#{var.type.to_ruby})"
      end.join("\nreader.next()\n")
    end
  end
end
