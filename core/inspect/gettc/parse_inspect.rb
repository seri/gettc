require "yaml"

require "gettc/parse"
require "gettc/print"
include Gettc

require_relative "helper"

def filename(dir_name, prob_name, extension)
  File.join(File.dirname(__FILE__), "../../temp/#{dir_name}/#{prob_name}.#{extension}")
end

def parse_all(name)
  benchmark "Parsing #{name}" do
    write_unless_exists(filename("parse_problem", name, "yml")) do |file|
      file.write($parser.parse(File.read(filename("download_problem_statement", name, "htm"))))
    end
  end
end

def main
  downloader = Downloader.new(Account.new("gettc", "algorithm"))
  $parser = Parser.new(downloader)

  parse_all("CirclesCountry")
  parse_all("PageNumbers")
  parse_all("TheTournamentDivOne")
  parse_all("FunnyGames")
  parse_all("BuildingRoads")
  parse_all("Acronyms")
  parse_all("BackyardTrees")
  parse_all("Bits")
  parse_all("CircularSequence")
  parse_all("PalindromeFactory")
  parse_all("TwistedMatrix")
  parse_all("Checker")
end

main
