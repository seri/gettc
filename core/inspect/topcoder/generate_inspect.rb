require 'tmpdir' 
require 'fileutils' 
require 'yaml' 

require 'topcoder/parse' 
require 'topcoder/generate'
include TopCoder

def get_prob name
    filename = File.join File.dirname(__FILE__), 
                         "../../files/yamls/#{name}.yml"    
    if not File.exists? filename then
        puts "#{filename} doesn't exist. Run parse_inspect first.'"
    end 
    return YAML.load File.open filename
end

def generate_all name
    print "Generating problem directory for #{name} ... "
    $generator.generate get_prob name
    puts 'Done'
end

def main
    source_d = File.join File.dirname(__FILE__), '../../template'
    target_d = File.join Dir.tmpdir, 'gettc'
    FileUtils.mkdir target_d if not File.exists? target_d
    $generator = Generator.new source_d, target_d

    generate_all 'CirclesCountry'
    generate_all 'PageNumbers'
    generate_all 'TheTournamentDivOne'
    generate_all 'FunnyGames'
    generate_all 'BuildingRoads'
end
main
