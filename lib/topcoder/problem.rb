module TopCoder
    class Case
        attr_accessor :input, :output, :reason
        def initialize
            @input = ''
            @output = ''
            @reason = ''
        end
    end
    class Image
        attr_accessor :name, :content
        def initialize
            @name = name
            @content = content
        end
    end
    class Problem
        attr_accessor :name, :url, :source, :statement, :definitions, :notes, 
                      :constraints, :examples, :systests, :images
        def initialize
            @name = ''
            @url = ''
            @source = ''
            @statement = ''
            @definitions = { }
            @notes = []
            @constraints = []
            @examples = []
            @systests = []
            @images = []
        end       
    end
end
