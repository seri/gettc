require 'topcoder/types'
    
module TopCoder 
    class SignatureError < StandardError
    end
    class CannotParseSignature < SignatureError
        attr_accessor :source
        def initialize source, msg = 'Cannot parse signature'
            @source = source
            super "#{msg} (#{source}"
        end
    end
    class InvalidVariableName < SignatureError
        attr_accessor :name
        def initialize name, msg = 'Invalid variable name'
            @name = name
            super "#{msg} (#{name})"            
        end
    end
    class Signature
        attr_accessor :type, :name
        def initialize type, name
            @type = type
            @name = name
        end
    end
    def parse_signature str
        str.strip!
        parts = str.split
        raise CannotParseSignature.new str if parts.size != 2
        type = parse_type parts[0]
        name = parts[1]
        if name =~ /^[a-zA-Z_]\w*$/
            return Signature.new type, name            
        else
            raise InvalidVariableName.new name
        end
    end
    def parse_method_signature str
        str.strip!        
        sigs = []
        parts = str.split '('
        raise CannotParseSignature.new str if parts.size != 2
        sigs << parse_signature(parts[0])

        str = parts[1]
        raise CannotParseSignature.new str if str[-1] != ')'
        str.chop!      

        parts = str.split ','
        parts.each do |sig| sigs << parse_signature(sig) end

        return sigs
    end
end
