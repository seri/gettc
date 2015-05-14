require "gettc/types"
include Gettc

module TopCoder
    class ParseError < StandardError
        def initialize text, pos, info = ""
            message = text
            if pos < text.size && pos >= 0
                message = pos == 0 ? "" : text[0..(pos - 1)]
                message << "|"
                message << text[pos]
                message << "|"
                message << text[(pos + 1)..(-1)]
            end 
            message = "<#{message}>"
            if info
                message += " (" + info + ")"
            end 
            super message            
        end
    end

    class Reader
        def initialize text
            if !text.is_a? String
                raise TypeError.new "need a string to construct a Reader"
            end 
            @text = text
            @pos = 0
            @len = @text.size
        end
        def next typ = nil
            if typ.nil?
                spaces
                expect ?,
                return                 
            end 

            case typ
            when TBoolean
                return next_boolean
            when TInt, TLong
                return next_int
            when TFloat, TDouble
                return next_float
            when TChar
                return next_char
            when TString
                return next_string
            else 
                if typ.is_a? TArray
                    return next_array typ.subtype
                end
            end

            raise UnsupportedType.new typ
        end

    private
        def raise_here message = nil
            raise ParseError.new @text, @pos, message
        end
        def check_pos
            if @pos >= @len
                raise_here "unexpected end of input"
            end             
        end

        def token
            check_pos
            return @text[@pos]
        end
        def spaces
            while @pos < @len && @text[@pos] =~ /\s/ do
                @pos += 1
            end
        end
        def expect char
            if token == char
                @pos += 1
            else
                raise_here "expecting <#{char}>"
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
            raise_here "expecting either true or false"
        end

        def is_digit
            return @text[@pos] =~ /\d/
        end
        def next_digits
            check_pos
            if !is_digit
                raise_here "expecting a digit"                
            end 
            start = @pos
            while true do
                @pos += 1
                if @pos == @len || !is_digit
                    break
                end 
            end
            return @text[start..(@pos - 1)]
        end

        def next_positive_int
            return next_digits.to_i
        end
        def next_int
            spaces
            if token == ?-
                @pos += 1
                return -next_positive_int
            end 
            return next_positive_int
        end

        def next_positive_float
            str = next_digits
            if @pos < @len
                if @text[@pos] == ?.
                    @pos += 1
                    str << "." << next_digits
                end 
            end 
            return str.to_f
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
                expect ?'
                return ret
            end 
            @pos += 1
            return char
        end

        def next_string
            spaces
            expect ?"
            start = @pos
            while true do
                if @pos >= @len
                    raise_here "expecting a closing quote when reading a string"
                end 
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

        def next_elems elem_type, arr
            spaces
            char = token
            case char
            when ?]
                @pos += 1
                return
            when ?,
                @pos += 1
                arr << self.next(elem_type)
                next_elems elem_type, arr
            else
                raise_here "expecting either <,> or <]>"
            end
        end

        def next_array elem_type
            arr = []
            spaces
            expect ?[
            spaces
            if token == ?]
                @pos += 1
                return arr
            end 
            arr << self.next(elem_type)
            next_elems elem_type, arr
            return arr
        end
    end
end
