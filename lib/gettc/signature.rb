require "gettc/types"

module Gettc
  class SignatureError < StandardError
  end

  class CannotParseSignature < SignatureError
    attr_accessor :source

    def initialize(source, message = "Cannot parse signature")
      @source = source
      super "#{message}: (#{source}"
    end
  end

  class InvalidVariableName < SignatureError
    attr_accessor :name

    def initialize(name, message = "Invalid variable name")
      @name = name
      super "#{message} (#{name})"
    end
  end

  class Signature
    attr_accessor :type, :name

    def initialize(type, name)
      @type = type
      @name = name
    end
  end

  def parse_signature(str)
    str.strip!

    parts = str.split
    raise CannotParseSignature.new(str) unless parts.size == 2

    type = parse_type(parts[0])
    name = parts[1]

    raise InvalidVariableName.new(name) unless name =~ /^[a-zA-Z_]\w*$/
    Signature.new(type, name)
  end

  def parse_method_signature(str)
    str.strip!

    sigs = []

    parts = str.split("(")
    raise CannotParseSignature.new(str) unless parts.size == 2
    sigs << parse_signature(parts[0])

    str = parts[1]
    raise CannotParseSignature.new(str) unless str[-1] == ")"

    str.chop.split(",").each { |sig| sigs << parse_signature(sig) }

    sigs
  end
end
