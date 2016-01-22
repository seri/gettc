require "gettc/problem"
require "gettc/download"

require "uri"
require "pathname"
require "hpricot"

module Gettc
  class Parser
    def initialize(downloader)
      @downloader = downloader
      @images = []
    end

    def parse(statement_html)
      doc = Hpricot(statement_html)

      prob = Problem.new
      prob.name = parse_name(doc.search("tr/td.statTextBig").html)
      prob.notes = nil
      prob.constraints = nil
      prob.examples = nil

      html = doc.search("td.problemText/table").html

      _, x = indexes(html, h3("Problem Statement"))
      y, z = indexes(html, h3("Definition"))
      prob.statement = parse_statement(html[x .. y])

      x, y = indexes(html, h3("Notes"))
      if x.nil?
        prob.notes = []
        x, y = indexes(html, h3("Constraints"))
        if x.nil?
          prob.constraints = []
          x, y = indexes(html, h3("Examples"))
          if x.nil?
            prob.examples = []
            x = -2
          end
        end
      end

      prob.definitions = parse_definitions(html[z .. x])

      if prob.notes.nil?
        z, x = indexes(html, h3("Constraints"))
        if z.nil?
          prob.constraints = []
          z, x = indexes(html, h3("Examples"))
          if z.nil?
            prob.examples = []
            z = - 2
          end
        end
        prob.notes = parse_notes(html[y .. z])
        x, y = z, x
      end

      if prob.constraints.nil?
        z, x = indexes(html, h3("Examples"))
        if z.nil?
          prob.examples = []
          z = -2
        end
        prob.constraints = parse_constraints(html[y .. z])
      end

      prob.examples = parse_examples(html[x .. -2]) if prob.examples.nil?

      prob.images = @images
      prob.url, prob.source, prob.systests = parse_details(doc)

      prob
    end

    private

    ## General helpers
    ## ===============

    def indexes(str, substr)
      from = str.index(substr)
      return nil if from.nil?
      return from - 1, from + substr.size
    end

    def filter html
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
          @images << image
          "![image](images/#{image.name})"
        rescue StandardError
          "![image](#{url})"
        end
      end

      text
    end

    def h3(tag)
      "<h3>#{tag}</h3>"
    end

    ## Parse problem parts
    ## ===================

    def parse_name(html)
      filter(html.sub("Problem Statement for", ""))
    end

    def parse_statement(html)
      filter(html)
    end

    def parse_definitions(html)
      Hpricot(html).search("/tr/td.statText/table/tr").each_with_object({}) do |tr, memo|
        tds = tr.search("/td.statText")
        memo[tds[0].to_plain_text[0 .. -2]] = tds[1].to_plain_text if tds.size == 2
      end
    end

    def parse_notes(html)
      notes = []
      Hpricot(html).search("/tr") do |tr|
        tds = tr.search("/td.statText")
        notes << filter(tds[1].html) if tds.size == 2
      end
      notes
    end

    def parse_constraints(html)
      return parse_notes(html)
    end

    ## @section Parse cases

    def filter_inout(text)
      text.gsub("{", "[").gsub("}", "]").strip
    end

    def parse_input(html)
      text = Hpricot(html).search("/table/tr/td.statText").each_with_object("") do |td, memo|
        input = td.to_plain_text.strip
        if memo.empty?
          memo = input
        else
          memo << ",\n" << input
        end
      end
      filter_inout(text)
    end

    def parse_output(html)
      filter_inout(Hpricot(html).to_plain_text.sub("Returns: ", ""))
    end

    def parse_reason(html)
      filter(html)
    end

    def parse_examples(html)
      examples = []

      tds = Hpricot(html).search("/tr/td.statText/table/tr/td.statText")

      i = 0
      while i < tds.size do
        example = Case.new
        example.input = parse_input(tds[i].html)
        example.output = parse_output(tds[i += 1].html)
        example.reason = parse_reason(tds[i += 1].html)
        examples << example
        i += 1
      end

      examples
    end

    def parse_systests(html)
      _, y = indexes(html, "<!-- System Testing -->")
      z, _ = indexes(html, "<!-- End System Testing -->")
      return [] unless y && z

      Hpricot(html[y .. z]).search("/table/tr[@valign=top]").each_with_object([]) do |tr, memo|
        tds = tr.search("/td.statText")
        next unless tds.size == 3

        test = Case.new
        test.input = filter_inout(tds[0].to_plain_text)
        test.output = filter_inout(tds[1].to_plain_text)
        memo << test
      end
    end

    def download_systests(detail_url)
      detail_html = @downloader.download(detail_url)
      Hpricot(detail_html).search("a[@href^=/stat?c=problem_solution]") do |elem|
        solution_html = @downloader.download(elem.attributes["href"])
        systests = parse_systests(solution_html)
        return systests unless systests.empty?
      end
      []
    end

    def parse_details(doc)
      doc.search("a[@href^=/tc?module=ProblemDetail]") do |elem|
        url = URI.join(Downloader::ROOT, elem.attributes["href"]).to_s
        systests = download_systests(url)
        return url, filter(elem.html), systests unless systests.empty?
      end
      return "", "", []
    end
  end
end
