require "gettc/problem"
require "gettc/download"

require "uri"
require "pathname"
require "hpricot"
require "logger"

module Gettc
  class Parser
    def initialize(downloader)
      @downloader = downloader
      @problem = Problem.new
    end

    def parse(problem_id)
      doc = Hpricot(@downloader.download_statement(problem_id))

      @problem = Problem.new
      @problem.id = problem_id
      @problem.name = parse_name(doc.search("tr/td.statTextBig").html)

      html = doc.search("td.problemText/table").html

      has_notes = true
      has_constraints = true
      has_examples = true

      _, x = self.class.indexes(html, self.class.h3("Problem Statement"))
      y, z = self.class.indexes(html, self.class.h3("Definition"))
      parse_statement(html[x .. y])

      x, y = self.class.indexes(html, self.class.h3("Notes"))
      if x.nil?
        has_notes = false
        x, y = self.class.indexes(html, self.class.h3("Constraints"))
        if x.nil?
          has_constraints = false
          x, y = self.class.indexes(html, self.class.h3("Examples"))
          if x.nil?
            has_examples = false
            x = -2
          end
        end
      end

      parse_definitions(html[z .. x])

      if has_notes
        z, x = self.class.indexes(html, self.class.h3("Constraints"))
        if z.nil?
          has_constraints = false
          z, x = self.class.indexes(html, self.class.h3("Examples"))
          if z.nil?
            has_examples = false
            z = - 2
          end
        end
        parse_notes(html[y .. z])
        x, y = z, x
      end

      if has_constraints
        z, x = self.class.indexes(html, self.class.h3("Examples"))
        if z.nil?
          has_examples = false
          z = -2
        end
        parse_constraints(html[y .. z])
      end

      parse_examples(html[x .. -2]) if has_examples
      parse_details(doc)

      @problem
    end

    private

    def self.indexes(str, substr)
      from = str.index(substr)
      return nil if from.nil?
      return from - 1, from + substr.size
    end

    def self.h3(tag)
      "<h3>#{tag}</h3>"
    end

    def self.filter_inout(text)
      text.gsub("{", "[").gsub("}", "]").strip
    end

    def self.get_param_value(url, param_key)
      if (match_data = url.match("[&|&amp;]#{param_key}=(\\d+)"))
        match_data[1] if match_data.size > 1
      end
    end

    def filter(html)
      html = html.force_encoding("ISO-8859-1")
        .encode("utf-8", {
          invaid: :replace,
          undef: :replace,
          replace: ""
        })

      html.gsub!(/<b>(\w*)<\/b>/) { |match| "*#{$1}*" }
      html.gsub!(/<sup>(\w*)<\/sup>/) { |match| "^(#{$1})" }
      html.gsub!("&#160;", "")
      html.gsub!("&nbsp;", " ")

      text = Hpricot(html).to_plain_text
      text.gsub!(/\[img:(http:\/\/[^\]]*)\]/) do |match|
        url = $1
        image = Image.new
        image.name = Pathname.new(url).basename
        begin
          image.content = @downloader.download(url)
          @problem.images << image
          "![image](images/#{image.name})"
        rescue HttpError
          "![image](#{url})"
        end
      end

      text
    end

    def parse_list_table(html)
      result = []
      Hpricot(html).search("/tr").each do |tr|
        tds = tr.search("/td.statText")
        result << filter(tds[1].html) if tds.size == 2
      end
      result
    end

    def parse_name(html)
      @problem.name = filter(html.sub("Problem Statement for", ""))
    end

    def parse_statement(html)
      @problem.statement = filter(html)
    end

    def parse_definitions(html)
      Hpricot(html).search("/tr/td.statText/table/tr").each do |tr|
        tds = tr.search("/td.statText")
        @problem.definitions[tds[0].to_plain_text[0 .. -2]] = tds[1].to_plain_text if tds.size == 2
      end
    end

    def parse_notes(html)
      @problem.notes = parse_list_table(html)
    end

    def parse_constraints(html)
      @problem.constraints = parse_list_table(html)
    end

    def parse_input(html)
      text = ""
      Hpricot(html).search("/table/tr/td.statText") do |td|
        input = td.to_plain_text.strip
        if text.empty?
          text = input
        else
          text << ",\n" << input
        end
      end
      self.class.filter_inout(text)
    end

    def parse_output(html)
      self.class.filter_inout(Hpricot(html).to_plain_text.sub("Returns: ", ""))
    end

    def parse_reason(html)
      filter(html)
    end

    def parse_examples(html)
      tds = Hpricot(html).search("/tr/td.statText/table/tr/td.statText")
      i = 0
      while i < tds.size
        example = Case.new
        example.input = parse_input(tds[i].html)
        example.output = parse_output(tds[i += 1].html)
        example.reason = parse_reason(tds[i += 1].html)
        @problem.examples << example
        i += 1
      end
    end

    def parse_systests(html)
      _, y = self.class.indexes(html, "<!-- System Testing -->")
      z, _ = self.class.indexes(html, "<!-- End System Testing -->")
      return [] unless y && z

      Hpricot(html[y .. z]).search("/table/tr[@valign=top]").each_with_object([]) do |tr, memo|
        tds = tr.search("/td.statText")
        next unless tds.size == 3

        test = Case.new
        test.input = self.class.filter_inout(tds[0].to_plain_text)
        test.output = self.class.filter_inout(tds[1].to_plain_text)
        @problem.systests << test
      end
    end

    def download_systests(round_id)
      detail_html = @downloader.download_detail(@problem.id, round_id)

      Hpricot(detail_html).search("a[@href^=/stat?c=problem_solution]") do |elem|
        url = elem.attributes["href"]

        if solution_id = self.class.get_param_value(url, "cr")
          parse_systests(@downloader.download_solution(@problem.id, round_id, solution_id))
          return unless @problem.systests.empty?
        end
      end
    end

    def parse_details(doc)
      doc.search("a[@href^=/tc?module=ProblemDetail]") do |elem|
        @problem.url = elem.attributes["href"]
        @problem.source = filter(elem.html)

        if round_id = self.class.get_param_value(@problem.url, "rd")
          download_systests(round_id)
          return unless @problem.systests.empty?
        end
      end
    end
  end
end
