require "test_helper"
require "gettc/generate"
require "fileutils"

class GenerateTest < Test::Unit::TestCase
  def setup
    script_dir = File.dirname(__FILE__)

    @source_dir = File.join(script_dir, "../../dist")
    raise "#{@source_dir} not found. Consider running `rake dist` first." unless File.exists?(@source_dir)

    @target_dir = File.join(script_dir, "../data/generated")
    FileUtils.mkdir(@target_dir) unless File.directory?(@target_dir)

    @generator = Generator.new(@source_dir, @target_dir)
  end

  def test_initialize
    dump_name = "this_directory_must_not_exist"
    assert_raise SourceDirNotExist do Generator.new(dump_name, @target_dir) end
    assert_raise TargetDirNotExist do Generator.new(@source_dir, dump_name) end
  end

  def test_problem_dir_exists
    problem = Problem.new
    problem.name = "JustATest"

    problem_dir = File.join(@target_dir, problem.name)
    FileUtils.mkdir(problem_dir) unless File.exists?(problem_dir)

    assert_raise ProblemDirExists do @generator.generate(problem) end
    FileUtils.rmdir problem_dir
  end

  def test_generate_problems
    $problems.each do |hash|
      @problem = hash

      begin
        @generator.generate(read_problem_yaml(@problem["name"]))
      rescue ProblemDirExists
        puts "Skipped generating #{@problem['name']} because it's already generated."
      end

      assert_equal hash["images_count"], count_files("prob/images")
      assert_equal 2 * hash["examples_count"], count_files("data/demo")
      assert_equal 2 * hash["systests_count"], count_files("data/sys")

      assert_file_exists "util/check/check.cpp"
      assert_file_exists "util/check/Makefile"

      assert_file_exists "bin/runner.rb"
      assert_file_exists "bin/runner.sh"

      assert_file_exists "prob/#{@problem['name']}.html"
      assert_file_exists "prob/#{@problem['name']}.md"
    end
  end

  private

  def count_files(subdir)
    full_path = File.join(@target_dir, @problem["name"], subdir)
    Dir.new(full_path).entries.reject { |name| [".", ".."].include?(name) }.size
  end

  def assert_file_exists(filename)
    assert File.exists?(File.join(@target_dir, @problem["name"], filename))
  end
end
