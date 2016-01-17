require "fileutils"
require "yaml"

require "gettc/parse"
require "gettc/generate"
include Gettc

require_relative "helper"

def get_prob name
  YAML.load(File.open(File.join(File.dirname(__FILE__), "../../temp/parse_problem/#{name}.yml")))
end

def generate_all name
  benchmark "Generating problem directory for #{name}" do
    $generator.generate(get_prob(name))
  end
end

def main
  config_dir = File.join(File.dirname(__FILE__), "../../dist")
  target_dir = File.join(File.dirname(__FILE__), "../../temp/generate_problem_dir")

  $generator = Generator.new(config_dir, target_dir)

  generate_all("CirclesCountry")
  generate_all("PageNumbers")
  generate_all("TheTournamentDivOne")
  generate_all("FunnyGames")
  generate_all("BuildingRoads")
end

main
