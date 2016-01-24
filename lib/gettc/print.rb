require "gettc/problem"
require "rdiscount"

module Gettc
  class Problem
    def to_md
      out  = "# [#{@name}](#{@url})\n"
      out << "*#{@source}*\n\n"

      out << "## Statement\n"
      out << @statement << "\n\n"

      unless @definitions.empty?
        out << "## Definitions\n"
        @definitions.each { |k, v| out << "- *#{k}*: `#{v}`\n" }
        out << "\n"
      end

      unless @notes.empty?
        out << "## Notes\n"
        @notes.each { |note| out << "- #{note}\n" }
        out << "\n"
      end

      unless @constraints.empty?
        out << "## Constraints\n"
        @constraints.each { |constraint| out << "- #{constraint}\n" }
        out << "\n"
      end

      unless @examples.empty?
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
          out << "#### Reason\n#{example.reason}\n\n" unless example.reason.empty?
        end
      end

      out
    end

    def to_html
      RDiscount.new(to_md).to_html
    end
  end
end
