require "gettc/types"

module Gettc
  class Type
    def dumb_python
      return "[]" if self.is_a?(TArray)

      case self
      when TInt, TLong, TFloat, TDouble
        "0"
      when TChar
        "'$'"
      when TString
        '"$"'
      when TBoolean
        "True"
      else
        "None"
      end
    end
  end

  class PythonEngine
    attr_reader :arglist, :input, :shebang

    def initialize func, vars
      @arglist = vars.map(&:name).join(", ")

      @input = vars.map do |var|
        "#{var.name} = reader.next(\"#{var.type}\")"
      end.join("\nreader.next()\n")

      @shebang = "#! #{python3_path}"
    end

    private

    def python3_path
      default = "/usr/bin/env python"

      version = system_q("python --version").split
      return default if version.size != 2 || version[0].upcase != "PYTHON"

      version = version[1].split(".")[0].to_i
      return default if version >= 3

      system_q("which python3") || default
    rescue
      return default
    end

    def system_q command
      IO.popen(command, err: [:child, :out]) { |f| f.gets }
    end
  end
end
