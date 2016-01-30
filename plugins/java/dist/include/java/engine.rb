require "gettc/types"

module Gettc
  class Type
    def to_java
      to_s
    end

    def to_java_boxed
      return "List<#{subtype.to_java_boxed}>" if self.is_a?(TArray)

      case self
      when TInt
        "Integer"
      when TLong
        "Long"
      when TFloat
        "Float"
      when TDouble
        "Double"
      when TChar
        "Character"
      when TString
        "String"
      when TBoolean
        "Boolean"
      else
        "Unknown"
      end
    end

    def dumb_java
      return "new #{subtype.to_java}[1]" if self.is_a?(TArray)

      case self
      when TInt, TLong, TFloat, TDouble
        "0"
      when TChar
        "'$'"
      when TString
        '"$"'
      when TBoolean
        "true"
      else
        "Nil"
      end
    end
  end

  class Signature
    def to_java
      "#{type.to_java} #{name}"
    end
  end

  class JavaArray
    attr_reader :name, :boxed_name, :type, :boxed_type

    def self.get_depth(type)
      type.is_a?(TArray) ? 1 + get_depth(type.subtype) : 0
    end

    def self.get_elem_type(type)
      type.is_a?(TArray) ? get_elem_type(type.subtype) : type
    end

    def initialize(sig)
      @sig = sig
      @name = sig.name
      @boxed_name = "#{@name}Boxed"
      @type = sig.type.to_java
      @boxed_type = sig.type.to_java_boxed
      @depth = self.class.get_depth(sig.type)
      @elem_type = self.class.get_elem_type(sig.type).to_java
    end

    def unbox_code
      ret = ""

      counters = @depth.times.map { |i| "_" + ("i"[0].ord + i).chr }
      indents = 0.upto(@depth).map { |i| " " * 4 * i }
      sizes = @depth.times.map { |i| @boxed_name + (".get(0)" * i) + ".size()" }

      ret << @type << " " << @name << " = new " << @elem_type
      @depth.times { |i| ret << "[" << sizes[i] << "]" }
      ret << ";\n"

      @depth.times do |i|
        ret << indents[i] << "for (int " << counters[i] << " = 0; "
        ret << counters[i] << " < " << sizes[i]
        ret << "; ++" << counters[i] << ")\n"
      end

      ret << indents[@depth] << @name
      @depth.times { |i| ret << "[" << counters[i] << "]" }
      ret << " = "

      ret << @boxed_name
      @depth.times { |i| ret << ".get(" << counters[i] << ")" }
      ret << ";"

      ret
    end

    def box_code_recurse(boxed_type, parent, boxed_name, depth, counters)
      ret = ""

      index = counters.map { |counter| "[#{counter}]" }.join

      if depth == 0
        ret << parent << ".add(" << @name << index << ");\n"
      else
        type_str = boxed_type.to_java_boxed
        ret << type_str << " " << boxed_name
        ret << " = new Array" << type_str << "();" << "\n"

        counter = "_" + ("i"[0].ord + counters.size).chr
        ret << "for (int " << counter << " = 0; "
        ret << counter << " < " << @name << index << ".length; "
        ret << "++" << counter << ") {\n"

        ret << box_code_recurse(boxed_type.subtype,
                                boxed_name,
                                boxed_name + counter,
                                depth - 1, counters << counter)
        ret << "}\n"

        unless parent.nil?
          ret << parent << ".add("
          ret << boxed_name  << ");\n"
        end
      end

      parent.nil? ? ret : ret.gsub(/^/, " " * 4)
    end

    def box_code
      box_code_recurse(@sig.type, nil, @boxed_name, @depth, [])
    end
  end

  class JavaEngine
    def initialize func, vars
      @func = func
      @vars = vars
    end

    def declare
      ret = "public "
      ret << @func.to_java << "("
      indent = " " * ret.size
      ret << @vars.map(&:to_java).join(",\n#{indent}")
      ret << ")"
    end

    def input
      @vars.map do |var|
        ret = ""
        if var.type.is_a?(TArray)
          arr = JavaArray.new(var)
          ret << arr.boxed_type
          ret << " " << arr.boxed_name
          ret << " = (" << arr.boxed_type << ") "
          ret << "reader.next(new TypeRef<" << arr.boxed_type << ">"
          ret << "(){}.getType());\n"
          ret << arr.unbox_code
        else
          ret << var.to_java
          ret << " = (" << var.type.to_java_boxed << ") "
          ret << "reader.next("
          ret << var.type.to_java_boxed
          ret << ".class);"
        end
        ret
      end.join("\nreader.next();\n\n")
    end

    def output
      ret = ""

      solver = "solver." + @func.name + "("
      solver << @vars.map(&:name).join(", ") << ")"

      if @func.type.is_a?(TArray)
        ret << @func.type.to_java << " result = " << solver << ";\n"
        arr = JavaArray.new(Signature.new(@func.type, "result"))
        ret << arr.box_code
        ret << "writer.write(resultBoxed);"
      else
        ret << "writer.write(" << solver << ");"
      end

      ret
    end
  end
end
