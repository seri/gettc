require 'topcoder/problem'
require 'bluecloth'

module TopCoder
    class Problem
        def to_md
            out  = "# [#{@name}](#{@url})\n"
            out << "*#{@source}*\n\n"

            out << "## Statement\n"
            out << @statement << "\n\n"

            if not @definitions.empty? then
                out << "## Definitions\n"
                @definitions.each do |k, v| out << "- *#{k}*: `#{v}`\n" end 
                out << "\n"
            end 

            if not @notes.empty? then
                out << "## Notes\n"
                @notes.each do |note| out << "- #{note}\n" end 
                out << "\n"
            end 

            if not @constraints.empty? then
                out << "## Constraints\n"
                @constraints.each do |constraint|
                    out << "- #{constraint}\n"
                end 
                out << "\n"
            end 

            if not @examples.empty? then
                out << "## Examples\n"
                @examples.each_index do |i|
                    example = @examples[i]
                    out << "### Example #{i + 1}\n"
                    out << "#### Input\n<c>"
                    out << example.input.gsub("\n", '<br />')
                    out << "</c>\n"
                    out << "#### Output\n<c>"
                    out << example.output.gsub("\n", '<br />')
                    out << "</c>\n"
                    if not example.reason.empty? then
                        out << "#### Reason\n#{example.reason}\n\n"
                    end 
                end
            end 
            return out
        end
        def to_html
            bc = BlueCloth.new to_md
            return bc.to_html
        end
    end
end
