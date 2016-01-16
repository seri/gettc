module Gettc
  TypeError = Class.new(StandardError)

  class UnsupportedType < TypeError
    attr_accessor :type

    def initialize(type = nil, msg = "Not a valid TopCoder type")
      @type = type
      super "#{msg} (#{type})"
    end
  end

  class Type
    def initialize(is_object)
      @is_object = is_object
    end

    def obj?
      @is_object
    end

    def to_s
      return "#{subtype}[]" if self.is_a?(TArray)

      case self
      when TInt
        "int"
      when TLong
        "long"
      when TFloat
        "float"
      when TDouble
        "double"
      when TChar
        "char"
      when TString
        "String"
      when TBoolean
        "boolean"
      else
        "unknown"
      end
    end
  end

  TBoolean = Type.new(false)
  TInt = Type.new(false)
  TLong = Type.new(false)
  TFloat = Type.new(false)
  TDouble = Type.new(false)
  TChar = Type.new(false)
  TString = Type.new(true)

  class TArray < Type
    attr_accessor :subtype

    def initialize(subtype)
      raise UnsupportedType.new(subtype) unless subtype.is_a?(Type)
      @subtype = subtype
    end

    def ==(ary)
      ary.is_a?(TArray) && @subtype == ary.subtype
    end

    def obj?
      true
    end
  end

  def parse_type(str)
    return TArray.new(parse_type(str[0 .. -3])) if str[-2 .. -1] == "[]"

    case str
    when "boolean"
      TBoolean
    when "int"
      TInt
    when "long"
      TLong
    when "float"
      TFloat
    when "double"
      TDouble
    when "char"
      TChar
    when "String"
      TString
    else
      raise UnsupportedType.new(str)
    end
  end
end
