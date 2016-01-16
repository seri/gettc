require "gettc/problem"
require "rdiscount"

module Gettc
  class Problem
    def to_md
      out  = "# [#{@name}](#{@url})\n"
      out << "*#{@source}*\n\n"

      out << "## Statement\n"
      out << @statement << "\n\n"

      unless @definitions.empty? then
        out << "## Definitions\n"
        @definitions.each do |k, v| out << "- *#{k}*: `#{v}`\n" end 
        out << "\n"
      end 

      unless @notes.empty? then
        out << "## Notes\n"
        @notes.each do |note| out << "- #{note}\n" end 
        out << "\n"
      end 

      unless @constraints.empty? then
        out << "## Constraints\n"
        @constraints.each do |constraint|
          out << "- #{constraint}\n"
        end 
        out << "\n"
      end 

      unless @examples.empty? then
        out << "## Examples\n"
        @examples.each_index do |i|
          example = @examples[i]
          out << "### Example #{i + 1}\n"
          out << "#### Input\n<c>"
          out << example.input.gsub("\n", "<br />")
          out << "</c>\n"
          out << "#### Output\n<c>"
          out << example.output.gsub("\n", "<br />")
          out << "</c>\n"
          unless example.reason.empty? then
            out << "#### Reason\n#{example.reason}\n\n"
          end 
        end
      end 
      return out
    end
    def to_html
      markdown = RDiscount.new to_md
      return markdown.to_html
    end
  end
end
