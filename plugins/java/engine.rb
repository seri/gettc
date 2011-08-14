require 'topcoder/types' 

module TopCoder
    class Type
        def to_java
            if self == TInt then
                return 'int'
            elsif self == TLong then
                return 'long'        
            elsif self == TFloat then
                return 'float'
            elsif self == TDouble then
                return 'double'
            elsif self == TChar then
                return 'char'
            elsif self == TString then
                return 'String'
            elsif is_a? TArray then
                return "#{subtype.to_java}[]"
            else
                return 'unknown'
            end 
        end
        def to_java_boxed
            if self == TInt then
                return 'Integer'
            elsif self == TLong then
                return 'Long'        
            elsif self == TFloat then
                return 'Float'
            elsif self == TDouble then
                return 'Double'
            elsif self == TChar then
                return 'Character'
            elsif self == TString then
                return 'String'
            elsif self.is_a? TArray then
                return "List<#{subtype.to_java_boxed}>"
            else
                return 'Unknown'
            end 
        end
        def dumb_java
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
            elsif self.is_a? TArray then
                return "new #{subtype.to_java}[1]"
            else
                return 'Nil'
            end
        end
    end
    class Signature
        def to_java
            return "#{type.to_java} #{name}"
        end
    end
    class JavaArray
        def self.get_depth type
            return 0 if not type.is_a? TArray
            return 1 + get_depth(type.subtype)
        end
        def self.get_elem_type type
            return type if not type.is_a? TArray
            return get_elem_type type.subtype
        end
        attr_accessor :name, :boxed_name, :type, :boxed_type
        def initialize sig
            @sig = sig
            @name = sig.name
            @boxed_name = "#{@name}Boxed"
            @type = sig.type.to_java
            @boxed_type = sig.type.to_java_boxed
            @depth = self.class.get_depth sig.type
            @elem_type = self.class.get_elem_type(sig.type).to_java
        end
        def unbox_code
            counters = @depth.times.map do |i| '_' + ('i'[0].ord + i).chr end
            indents = 0.upto(@depth).map do |i| ' ' * 4 * i end
            sizes = @depth.times.map do |i|
                @boxed_name + ('.get(0)' * i) + '.size()'
            end
            ret = ''

            ret << @type << ' ' << @name << ' = new ' << @elem_type;
            @depth.times do |i| ret << '[' << sizes[i] << ']' end
            ret << ";\n"
        
            @depth.times do |i|
                ret << indents[i] << 'for (int ' << counters[i] << ' = 0; '
                ret << counters[i] << ' < ' << sizes[i]
                ret << '; ++' << counters[i] << ")\n"
            end

            ret << indents[@depth] << @name
            @depth.times do |i| ret << '[' << counters[i] << ']' end            
            ret << ' = '
            ret << @boxed_name
            @depth.times do |i| ret << '.get(' << counters[i] << ')' end            
            ret << ";"

            return ret
        end
        def box_code_recurse boxed_type, parent, boxed_name, depth, counters
            ret = ''
            index = counters.map do |counter| '[' + counter + ']' end
            index = index.join
            if depth == 0
                ret << parent << '.add(' << @name << index << ");\n"
            else
                type_str = boxed_type.to_java_boxed
                ret << type_str << ' ' << boxed_name 
                ret << ' = new Array' << type_str << '();' << "\n"

                counter = '_' + ('i'[0].ord + counters.size).chr
                ret << 'for (int ' << counter << ' = 0; '
                ret << counter << ' < ' << @name << index << '.length; '
                ret << '++' << counter << ") {\n"

                ret << box_code_recurse(boxed_type.subtype, 
                                        boxed_name, 
                                        boxed_name + counter, 
                                        depth - 1, counters << counter)
                ret << "}\n"

                if not parent.nil?
                    ret << parent << '.add(' 
                    ret << boxed_name  << ");\n"
                end
            end
            ret.gsub! /^/, ' ' * 4 if not parent.nil? 
            return ret
        end
        def box_code
            box_code_recurse @sig.type, nil, @boxed_name, @depth, []
        end 
    end
    class JavaEngine
        attr_accessor :func, :vars
        def initialize func, vars
            @func = func
            @vars = vars            
        end
        def declare 
            ret = 'public '
            ret << @func.to_java << '('
                indent = ' ' * ret.size
                temp = @vars.map do |var| var.to_java end
                ret << temp.join(",\n#{indent}")
            ret << ')'
            return ret
        end
        def input
            temp = @vars.map do |var| 
                ret = ''
                if var.type.is_a? TArray
                    arr = JavaArray.new var
                    ret << arr.boxed_type
                    ret << ' ' << arr.boxed_name
                    ret << ' = (' << arr.boxed_type << ') '
                    ret << 'reader.next(new TypeRef<' << arr.boxed_type << '>'
                    ret << "(){}.getType());\n"
                    ret << arr.unbox_code
                else
                    ret << var.to_java
                    ret << ' = (' << var.type.to_java_boxed << ') '
                    ret << 'reader.next(' 
                    ret << var.type.to_java_boxed 
                    ret << '.class);'
                end
                ret
            end
            return temp.join "\nreader.next();\n\n"
        end
        def output
            ret = ''
            caller = 'solver.' + @func.name + '('
            temp = @vars.map do |var| var.name end
            caller << temp.join(', ') << ')'
            if @func.type.is_a? TArray
                ret << @func.type.to_java << ' result = ' << caller << ";\n"
                arr = JavaArray.new(Signature.new @func.type, 'result')
                ret << arr.box_code
                ret << 'writer.write(resultBoxed);'
            else
                ret << 'writer.write(' << caller << ');'
            end
            return ret
        end
    end
end
