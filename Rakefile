require "rubygems/package_task" 
require "rubygems/installer" 
require "rake/clean" 
require "fileutils"

require_relative "helper"

CLEAN << "pkg"
CLEAN << "temp"

spec = Gem::Specification.new do |s| 
    s.platform = Gem::Platform::RUBY
    s.name = "gettc"
    s.summary = "TopCoder offline arena supporting multiple languages"
    s.description = "Given a TopCoder problem ID, gettc downloads the problem specification, parses the whole thing into a Markdown file, generates inputs/outputs based on the Examples and System Tests given, and finally generates basic solution files for you to get started."
    s.version = "1.6"

    s.author = "Seri"
    s.email = "seritrinh@gmail.com"
    s.homepage = "http://seri.github.io/gettc"

    s.files = FileList["{bin,dist}/**/*"].include("core/lib/**/*").to_a
    s.test_files = FileList["core/test/**/*_test.rb"].to_a
    s.require_path = "core/lib"
    s.has_rdoc = false

    s.bindir = "bin"
    s.executables = ["gettc"]

    s.add_dependency "hpricot"
    s.add_dependency "rdiscount"
end 
Gem::PackageTask.new spec do |pkg|
    pkg.need_tar = true
end

desc "Inspect the main gettc script"
task :inspect do
    rm_rf "temp"
    mkdir "temp"
    with_env "GETTC_HOME", File.expand_path("dist") do
        with_dir "temp" do
            ruby "-I../core/lib ../bin/gettc 11127"
            chdir "DigitHoles/solve/cpp"
            sh "make demo"
            chdir "../haskell"
            sh "make demo"
            chdir "../java"
            sh "make demo"
            chdir "../python"
            sh "make demo"
        end
    end
end

desc "Merge the core and all plugins dists into a final dist"
task :dist do
    rm_rf "dist"
    command = "cp -r core/dist "
    dir = Dir.new "plugins"
    dir.each do |plugin|
        if plugin != "." and plugin != ".."
            dist = File.join "plugins", plugin, "dist"
            if File.exists? dist
                command << dist << " "
            end
        end
    end
    command << " -t ." 
    sh command
end