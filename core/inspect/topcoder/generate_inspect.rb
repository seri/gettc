require "fileutils" 
require "yaml" 

require "topcoder/parse" 
require "topcoder/generate"
include TopCoder

require_relative "helper"

def get_prob name
    filename = File.join File.dirname(__FILE__), 
                         "../../files/yamls/#{name}.yml"    
    return YAML.load File.open filename
end

def generate_all name
    benchmark "Generating problem directory for #{name}" do
        $generator.generate get_prob name
    end
end

def main
    source_d = File.join File.dirname(__FILE__), "../../../dist/template"
    target_d = File.join File.dirname(__FILE__), "../../files/results"
    FileUtils.mkdir target_d unless File.exists? target_d
    $generator = Generator.new source_d, target_d

    generate_all "CirclesCountry"
    generate_all "PageNumbers"
    generate_all "TheTournamentDivOne"
    generate_all "FunnyGames"
    generate_all "BuildingRoads"
end
main
