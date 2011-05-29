module TopCoder
    class Example
        attr_accessor :input, :output, :reason
        def initialize
            @input = ''
            @output = ''
            @reason = ''
        end
    end
    class Problem
        attr_accessor :statement, :definitions, :notes, :constraints, :examples
        def initialize
            @statement = ''
            @definitions = { }
            @notes = []
            @constraints = []
            @examples = []
        end        
        def to_s
            out  = "Problem Statement\n"
            out << "=================\n\n"
            out << @statement << "\n\n"

            out << "Definition\n"
            out << "==========\n\n"
            @definitions.each do |k, v|
                out << "#{k}: #{v}\n"
            end 
            out << "\n"

            out << "Notes\n"
            out << "=====\n\n" 
            @notes.each do |note|
                out << "- #{note}\n"
            end 
            out << "\n"

            out << "Constraints\n"
            out << "===========\n\n"
            @constraints.each do |constraint|
                out << "- #{constraint}\n"
            end 
            out << "\n"

            out << "Examples\n"
            out << "========\n\n"
            @examples.each_index do |i|
                example = @examples[i]
                out << "Example #{i + 1}\n"
                out << "---------\n"
                out << "### Input\n#{example.input}\n"
                out << "### Output\n#{example.output}\n"
                out << "### Reason\n#{example.reason}\n\n"
            end

            return out
        end
    end
end
