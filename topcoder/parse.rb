require 'topcoder/problem.rb' 
require 'hpricot'

class String
    def indexes (substr)
        from = index(substr)
        return nil if from.nil?
        to = from + substr.size
        return from - 1, to
    end
end

module TopCoder
    class Parser
        def h3 tag
            return "<h3>#{tag}</h3>"
        end
        def get_statement html
            return Hpricot(html).to_plain_text
        end
        def get_definitions html
            defs = { }
            Hpricot(html).search '/table/tr' do |tr|
                tds = tr.search '/td.statText'
                if tds.size == 2 then
                    key = tds[0].to_plain_text[0 .. -2]
                    value = tds[1].to_plain_text
                    defs[key] = value
                end 
            end 
            return defs
        end
        def get_notes html
            notes = []
            Hpricot(html).search '/tr' do |tr|
                tds = tr.search '/td.statText'
                if tds.size == 2 then
                    notes << tds[1].to_plain_text
                end 
            end 
            return notes
        end
        def get_constraints html
            return get_notes html
        end
        def parse_input html
            text = nil
            Hpricot(html).search '/table/tr/td.statText' do |td|
                if text.nil? then
                    text = td.to_plain_text
                else
                    text << "\n" << td.to_plain_text
                end 
            end 
            return text
        end
        def parse_output html
            text = Hpricot(html).to_plain_text
            text.sub! 'Returns: ', ''
            return text.strip
        end
        def parse_reason html
            html.sub! '<sup>', '^'
            return Hpricot(html).to_plain_text.strip
        end
        def get_examples html
            examples = []
            tds = Hpricot(html).search('/tr/td.statText/table/tr/td.statText')
            i = 0
            while i < tds.size do
                example = Example.new
                example.input = parse_input tds[i].inner_html
                example.output = parse_output tds[i += 1].inner_html
                example.reason = parse_reason tds[i += 1].inner_html
                examples << example 
                i += 1
            end
            return examples
        end
        def parse html
            prob = Problem.new 
            prob.notes = nil
            prob.constraints = nil
            prob.examples = nil 

            html = Hpricot(html).at('td.problemText').at('table').inner_html

            _, x = html.indexes h3('Problem Statement')
            y, z = html.indexes h3('Definition')
            prob.statement = get_statement html[x .. y]

            x, y = html.indexes h3('Notes')
            if x.nil? then
                prob.notes = []
                x, y = html.indexes h3('Constraints')
                if x.nil? then
                    prob.constraints = []
                    x, y = html.indexes h3('Examples')
                    if x.nil? then
                        prob.examples = []
                        x = -2
                    end 
                end 
            end 
            prob.definitions = get_definitions html[z .. x]

            if prob.notes.nil? then
                z, x = html.indexes h3('Constraints')
                if z.nil? then
                    prob.constraints = []
                    z, x = html.indexes h3('Examples') 
                    if z.nil? then
                        prob.examples = []
                        z = - 2
                    end 
                end 
                prob.notes = get_notes html[y .. z]
                x, y = z, x
            end 

            if prob.constraints.nil? then
                z, x = html.indexes h3('Examples')
                if z.nil? then
                    prob.examples = []
                    z = -2
                end 
                prob.constraints = get_constraints html[y .. z]
            end 

            if prob.examples.nil? then
                prob.examples = get_examples html[x .. -2]
            end 

            return prob
        end    
    end
    def self.parse html
        parser = Parser.new
        return parser.parse html
    end
end
