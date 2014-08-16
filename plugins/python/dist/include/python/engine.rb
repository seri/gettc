require "gettc/types"

module Gettc
    class Type
        def dumb_python
            if self == TInt then
                return "0"
            elsif self == TLong then
                return "0"          
            elsif self == TFloat then
                return "0"
            elsif self == TDouble then
                return "0"
            elsif self == TChar then
                return "'$'"
            elsif self == TString then
                return '"$"'
            elsif self == TBoolean then
                return "True"
            elsif self.is_a? TArray then
                return "[]"
            else
                return "None"
            end
        end
    end
    class PythonEngine
        attr_reader :arglist, :input, :shebang
        def initialize func, vars
            temp = vars.map do |var|
                var.name
            end
            @arglist = temp.join ", "

            temp = vars.map do |var| 
                var.name + ' = reader.next("' + var.type.to_s + '")'
            end
            @input = temp.join "\nreader.next()\n"

            @shebang = "#! "
            @shebang << python3_path
        end
    private
        def python3_path
            default = "/usr/bin/env python"

            version = system_q("python --version").split
            if version.size != 2 || version[0].upcase != "PYTHON" then
                return default
            end

            version = version[1].split(".")[0].to_i
            if version >= 3 then
                return default
            end 

            python3 = system_q "which python3"
            return python3.empty? ? default : python3
        rescue
            return default
        end
        def system_q command
            IO.popen command, err: [:child, :out] do |f|
                return f.gets
            end
        end
    end
end
