require "yaml" 

require "topcoder/parse" 
require "topcoder/print" 
include TopCoder

require_relative "helper"

def filename dir_name, prob_name, ext
    File.join File.dirname(__FILE__), 
              "../../files/#{dir_name}/#{prob_name}.#{ext}"
end

def parse_all name
    benchmark "Parsing #{name}" do 
        statfile = filename("stats", name, "htm")
        html = File.open statfile, "r" do |f| f.read end 
        prob = $parser.parse html
        File.open filename("yamls", name, "yml"), "w" do |f| f.write prob.to_yaml end
    end
end

def main
    downloader = Downloader.new Account.new "gettc", "algorithm"
    $parser = Parser.new downloader

    parse_all "CirclesCountry"
    parse_all "PageNumbers"
    parse_all "TheTournamentDivOne"
    parse_all "FunnyGames"
    parse_all "BuildingRoads"
    parse_all "Acronyms"
    parse_all "BackyardTrees"
    parse_all "Bits"
    parse_all "CircularSequence"
    parse_all "PalindromeFactory"
    parse_all "TwistedMatrix"
    parse_all "Checker"
end
main
