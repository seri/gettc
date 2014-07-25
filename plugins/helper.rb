require "yaml"
require_relative "../helper"

$self_d = File.dirname __FILE__
$LOAD_PATH.unshift File.join $self_d, "../core/lib"
require "gettc"
include Gettc

$options = {
    verbose: false
}

def generate_problems
    probs = []
    arr = YAML.load_file File.join $self_d, "problems.yml"
    arr.each do |elem|
        prob = Problem.new
        prob.name = elem["name"]
        prob.definitions["Method signature"] = elem["signature"]

        example = Case.new
        example.input = elem["example"]["input"]
        example.output = elem["example"]["output"]

        prob.examples << example
        probs << prob
    end
    return probs
end

def generate_solutions plugin_d
    plugin_temp_d = File.join plugin_d, "temp"
    plugin_dist_d = File.join plugin_d, "dist"
    combined_dist_d = File.join plugin_temp_d, "dist"
    if File.exists? combined_dist_d then
        puts "It seems rake generate has already been executed."
        puts "Try rake clean and then rake generate to generate stuffs again."
        return
    end 

    puts "[gettc] Combining base dist and plugin dist"
    mkdir_p plugin_temp_d, $options
    command = "cp -r "
    command << plugin_dist_d << " "
    command << File.join($self_d, "base/dist") << " -t " << plugin_temp_d
    sh command, $options

    gen = Generator.new combined_dist_d, plugin_temp_d
    generate_problems.each do |prob|
        print "[gettc] Generating solution directory for #{prob.name} ... "
        gen.generate prob
        puts "Done"
    end
end

def try_run_solutions plugin_d
    plugin_name = File.basename plugin_d
    plugin_temp_d = File.join plugin_d, "temp"

    combined_dist_d = File.join plugin_temp_d, "dist"
    unless File.exists? combined_dist_d then
        puts "#{combined_dist_d} does not exist."
        puts "Always call rake generate before rake run."
        return
    end

    puts "Now attempt to call make in the generated solution for each problem."
    puts "Lots of output will follow. Look out for compiling/parsing error."
    puts "Also expect to see no recompilation in the second call to rake run."

    with_env "GETTC_HOME", combined_dist_d do 
        generate_problems.each do |prob|
            prob_d = File.join plugin_temp_d, prob.name
            puts "[gettc] Running the generated solution for problem #{prob.name}"
            unless File.exists? prob_d then
                puts "#{prob_d} does not exist."
                puts "Always call rake generate before rake run."
                return
            end
            with_dir "#{prob_d}/solve/#{plugin_name}" do
                sh "make --quiet demo", $options
                puts 
            end
        end
    end
end