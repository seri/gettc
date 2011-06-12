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
        attr_accessor :name, :url, :source, :statement, :definitions, :notes, 
                      :constraints, :examples
        def initialize
            @name = ''
            @url = ''
            @source = ''
            @statement = ''
            @definitions = { }
            @notes = []
            @constraints = []
            @examples = []
        end       
    end
end
