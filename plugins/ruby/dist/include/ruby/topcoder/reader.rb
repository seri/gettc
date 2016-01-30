require "gettc/types"
include Gettc

module TopCoder
  class ParseError < StandardError
    def initialize(text, pos, info = "")
      message = text

      if pos < text.size && pos >= 0
        message = pos == 0 ? "" : text[0..(pos - 1)]
        message << "|" << text[pos] << "|"
        message << text[(pos + 1)..(-1)]
      end

      message = "<#{message}>"
      message << " (#{info})" if info

      super message
    end
  end

  class Reader
    def initialize(text)
      raise ArgumentError.new("need a string to construct a Reader") unless text.is_a?(String)
      @text = text
      @pos = 0
      @len = @text.size
    end

    def next(type = nil)
      if type.nil?
        spaces
        expect(?,)
        return
      end

      return next_array(type.subtype) if type.is_a?(TArray)

      case type
      when TBoolean
        next_boolean
      when TInt, TLong
        next_int
      when TFloat, TDouble
        next_float
      when TChar
        next_char
      when TString
        next_string
      else
        raise UnsupportedType.new(type)
      end
    end

    private

    def raise_here(message = nil)
      raise ParseError.new(@text, @pos, message)
    end

    def check_pos
      raise_here("unexpected end of input") if @pos >= @len
    end

    def token
      check_pos
      @text[@pos]
    end

    def spaces
      while @pos < @len && @text[@pos] =~ /\s/ do
        @pos += 1
      end
    end

    def expect(char)
      if token == char
        @pos += 1
      else
        raise_here("expecting <#{char}>")
      end
    end

    def next_boolean
      spaces

      if @text[@pos, 4].upcase == "TRUE"
        @pos += 4
        return true
      elsif @text[@pos, 5].upcase == "FALSE"
        @pos += 5
        return false
      end

      raise_here("expecting either true or false")
    end

    def is_digit
      @text[@pos] =~ /\d/
    end

    def next_digits
      check_pos
      raise_here("expecting a digit") unless is_digit

      start = @pos
      while true do
        @pos += 1
        break if @pos == @len || !is_digit
      end

      @text[start..(@pos - 1)]
    end

    def next_positive_int
      next_digits.to_i
    end

    def next_int
      spaces

      if token == ?-
        @pos += 1
        return -next_positive_int
      end

      next_positive_int
    end

    def next_positive_float
      str = next_digits

      if @pos < @len
        if @text[@pos] == ?.
          @pos += 1
          str << "." << next_digits
        end
      end

      str.to_f
    end

    def next_float
      spaces

      if token == ?-
        @pos += 1
        return -next_positive_float
      end

      return next_positive_float
    end

    def next_char
      spaces
      char = token

      if char == ?'
        @pos += 1
        ret = token
        @pos += 1
        expect(?')
        return ret
      end

      @pos += 1
      char
    end

    def next_string
      spaces
      expect(?")
      start = @pos
      while true do
        raise_here("expecting a closing quote when reading a string") if @pos >= @len
        if token == ?"
          @pos += 1
          saved = @pos
          spaces
          if @pos == @len || @text[@pos] == ?, || @text[@pos] == ?]
            @pos = saved
            return @text[start..(@pos - 2)]
          end
        else
          @pos += 1
        end
      end
    end

    def next_elems(elem_type, arr)
      spaces
      char = token

      case char
      when ?]
        @pos += 1
        return
      when ?,
        @pos += 1
        arr << self.next(elem_type)
        next_elems(elem_type, arr)
      else
        raise_here("expecting either <,> or <]>")
      end
    end

    def next_array(elem_type)
      arr = []

      spaces
      expect(?[)
      spaces

      if token == ?]
        @pos += 1
        return arr
      end

      arr << self.next(elem_type)
      next_elems(elem_type, arr)

      arr
    end
  end
end
