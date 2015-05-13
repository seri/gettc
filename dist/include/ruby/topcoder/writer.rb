require "gettc/types"
include Gettc

module TopCoder
    class Writer
        def initialize
            @text = ""
        end
        def next value = nil, typ = nil
            if value.nil? && typ.nil?
                @text << ", "
                return
            end 

            case typ
            when TBoolean, TInt, TLong, TFloat, TDouble
                @text << value.to_s
            when TChar
                @text << ?' << value << ?'
            when TString
                @text << ?" << value << ?"
            else 
                if typ.is_a? TArray
                    @text << ?[
                    value.each_index do |i|
                        self.next value[i], typ.subtype
                        if i < value.size - 1
                            self.next
                        end 
                    end
                    @text << ?]
                else
                    raise UnrecognizedType.new typ
                end
            end
        end
        def to_s
            return @text
        end
    end
end
