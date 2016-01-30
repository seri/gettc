require "test/unit"
require "fileutils"
require "tmpdir"

require "gettc/generate"
include Gettc

class GenerateTest < Test::Unit::TestCase
  def setup
    @source_dir = File.join(File.dirname(__FILE__), "../../dist")
    @target_dir = File.join(Dir.tmpdir, "gettc")
    FileUtils.mkdir(@target_dir) unless File.directory?(@target_dir)
    @generator = Generator.new(@source_dir, @target_dir)
  end

  def test_initialize
    assert_raise SourceDirNotExist do Generator.new("this_directory_must_not_exist", @target_dir) end
    assert_raise TargetDirNotExist do Generator.new(@source_dir, "this_directory_must_not_exist") end
  end

  def test_prob_dir_exists
    prob = Problem.new
    prob.name = "JustATest"

    prob_dir = File.join(@target_dir, prob.name)
    FileUtils.mkdir(prob_dir) unless File.exists?(prob_dir)

    assert_raise ProblemDirExists do @generator.generate(prob) end
    FileUtils.rmdir prob_dir
  end
end
