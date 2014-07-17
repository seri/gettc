require 'yaml' 

require 'topcoder/parse' 
require 'topcoder/print' 
include TopCoder

def filename dir_name, prob_name, ext
    File.join File.dirname(__FILE__), 
              "../../files/#{dir_name}/#{prob_name}.#{ext}"
end

def parse_all name
    print "Parsing #{name} ... " 
    statfile = filename('stats', name, 'htm')
    unless File.exists? statfile then
        $stderr.puts "#{statfile} doesn't exist. Run download_inspect first."
        return
    end 
    html = File.open statfile, 'r' do |f| f.read end 
    prob = $parser.parse html
    File.open filename('yamls', name, 'yml'), 'w' do |f| f.write prob.to_yaml end
    puts "Done"
end

def main
    downloader = Downloader.new Account.new 'gettc', 'algorithm'
    $parser = Parser.new downloader

    #parse_all 'CirclesCountry'
    #parse_all 'PageNumbers'
    #parse_all 'TheTournamentDivOne'
    #parse_all 'FunnyGames'
    #parse_all 'BuildingRoads'
    #parse_all 'Acronyms'
    #parse_all 'BackyardTrees'
    #parse_all 'Bits'
    #parse_all 'CircularSequence'
    #parse_all 'PalindromeFactory'
    #parse_all 'TwistedMatrix'
    parse_all 'Checker'
end
main
