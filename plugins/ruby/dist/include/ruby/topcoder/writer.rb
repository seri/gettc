require "gettc/types"
include Gettc

module TopCoder
  class Writer
    def initialize
      @text = ""
    end

    def next(value = nil, type = nil)
      if value.nil? || type.nil?
        @text << ", "
        return self
      end

      if type.is_a?(TArray)
        @text << ?[

        value.each_index do |i|
          self.next(value[i], type.subtype)
          self.next if i < value.size - 1
        end

        @text << ?]
        return self
      end

      case type
      when TBoolean, TInt, TLong, TFloat, TDouble
        @text << value.to_s
      when TChar
        @text << ?' << value << ?'
      when TString
        @text << ?" << value << ?"
      else
        raise UnsupportedType.new(type)
      end

      return self
    end

    def to_s
      @text
    end
  end
end
