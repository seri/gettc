require 'topcoder/types' 
class String
    def uncapitalize
        return self[0, 1].downcase + self[1..-1]
    end
end
module TopCoder
    class Type
        def to_haskell
            if self == TInt then
                return 'Int'
            elsif self == TLong then
                return 'Integer'        
            elsif self == TFloat then
                return 'Float'
            elsif self == TDouble then
                return 'Double'
            elsif self == TChar then
                return 'Char'
            elsif self == TString then
                return 'String'
            elsif self == TBoolean then
                return 'Bool'
            elsif is_a? TArray then
                return '[' + subtype.to_haskell + ']'
            else
                return 'Unknown'
            end 
        end
        def get_haskell_parser
            if self == TInt then
                return 'parseInt'
            elsif self == TLong then
                return 'parseLong'        
            elsif self == TFloat then
                return 'parseFloat'
            elsif self == TDouble then
                return 'parseDouble'
            elsif self == TChar then
                return 'parseChar'
            elsif self == TString then
                return 'parseString'
            elsif self == TBoolean then
                return 'parseBool'
            elsif is_a? TArray then
                return '(parseList ' + subtype.get_haskell_parser + ')'
            else
                return 'unknown'
            end
        end
        def dumb_haskell
            if self == TInt then
                return '0'
            elsif self == TLong then
                return '0'        
            elsif self == TFloat then
                return '0'
            elsif self == TDouble then
                return '0'
            elsif self == TChar then
                return "'$'"
            elsif self == TString then
                return '"$"'
            elsif self == TBoolean then
                return 'True'
            elsif is_a? TArray then
                return '[]'
            else
                return 'Nil'
            end
        end
    end
    class HaskellEngine
        attr_accessor :func, :vars
        def initialize func, vars
            @func = Signature.new func.type, func.name.uncapitalize
            @vars = vars.map do |var|
                Signature.new var.type, var.name.uncapitalize
            end
        end
        def declare
            ret = ''
            ret << func.name << ' :: '

            temp = vars.map do |var| var.type.to_haskell end
            ret << temp.join(' -> ') << ' -> ' << func.type.to_haskell << "\n"

            ret << func.name << ' '
            temp = vars.map do |var| var.name end
            ret << temp.join(' ') << ' = ' << func.type.dumb_haskell

            return ret
        end
        def input
            ret = 'getVars :: Parser ('
            temp = @vars.map do |var| var.type.to_haskell end
            ret << temp.join(', ') << ")\n"
            temp = 'getVars = do '
            ret << temp
                indent = ' ' * temp.size
                temp = @vars.map do |var|
                    x = ''
                    x << var.name
                    x << ' <- spaces >> ' 
                    x << var.type.get_haskell_parser
                end
            ret << temp.join(" ; next\n#{indent}") << "\n"
            ret << indent << 'return ('
                temp = @vars.map do |var| var.name end
                ret << temp.join(', ')
            ret << ')'
            return ret
        end
        def output
            ret = ''
            ret << @func.name << ' '
            temp = vars.map do |var| var.name end
            ret << temp.join(' ')
            return ret
        end
    end
end
