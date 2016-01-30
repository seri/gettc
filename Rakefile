require "rubygems/package_task"
require "rubygems/installer"
require "rake/clean"
require "fileutils"

require_relative "helper"
require_relative "lib/version"

CLEAN << "pkg"
CLEAN << "temp"

spec = Gem::Specification.new do |s|
  s.platform = Gem::Platform::RUBY
  s.name = "gettc"
  s.summary = "TopCoder offline arena supporting multiple languages"
  s.description = "Given a TopCoder problem ID, gettc downloads the problem specification, parses the whole thing into a Markdown file, generates inputs/outputs based on the Examples and System Tests given, and finally generates basic solution files for you to get started."
  s.version = Gettc::VERSION

  s.author = "Seri"
  s.email = "seritrinh@gmail.com"
  s.homepage = "http://seri.github.io/gettc"

  s.files = FileList["{bin,dist}/**/*"].include("core/lib/**/*").to_a
  s.test_files = FileList["test/**/*_test.rb"].to_a
  s.require_path = "lib"
  s.has_rdoc = false

  s.bindir = "bin"
  s.executables = ["gettc"]

  s.add_dependency "hpricot"
  s.add_dependency "rdiscount"
end

Gem::PackageTask.new(spec) do |pkg|
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
      chdir "../go"
      sh "make demo"
    end
  end
end

desc "Merge the core and all plugins dists into a final dist"
task :dist do
  rm_rf "dist"
  sh "cp -R base dist"

  plugin_dists = Dir.new("plugins").map do |plugin_dir|
    if [".", ".."].include?(plugin_dir)
      ""
    else
      plugin_dist = File.join("plugins", plugin_dir, "dist")
      File.exists?(plugin_dist) ? plugin_dist : ""
    end
  end
  sh "cp -R #{plugin_dists.join(" ")} -t ."
end

file "test/data" do
  mkdir "test/data"
  mkdir "test/data/statement"
  mkdir "test/data/detail"
  mkdir "test/data/solution"
  mkdir "test/data/parsed"
  mkdir "test/data/generated"
end

def execute_test(name)
  ruby "-Ilib -Itest test/gettc/#{name}_test.rb"
end

namespace :test do
  desc "Test the types"
  task :types do
    execute_test("types")
  end

  desc "Test the signature"
  task :signature do
    execute_test("signature")
  end

  desc "Test the downloader"
  task download: "test/data" do
    execute_test("download")
  end

  desc "Test the parser"
  task :parse do
    if File.exists?("test/data/statement/PageNumbers.html")
      execute_test("parse")
    else
      puts "Run rake test:download first"
    end
  end

  desc "Test the generator"
  task :generate do
    execute_test("generate")
  end
end

task test: %w[test:types test:signature test:download test:parse test:generate] do
end
