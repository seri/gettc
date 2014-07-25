require "test/unit" 
require "fileutils"
require "tmpdir"

require "topcoder/generate" 
include TopCoder

class GenerateTest < Test::Unit::TestCase
    def setup
        @source_d = File.join File.dirname(__FILE__), "../../dist/template"
        @target_d = File.join Dir.tmpdir, "gettc"
        FileUtils.mkdir @target_d unless File.directory? @target_d
        @generator = Generator.new @source_d, @target_d
    end
    def test_initialize
        assert_raise SourceDirNotExist do 
            Generator.new "this_directory_must_not_exist", @target_d
        end
        assert_raise TargetDirNotExist do 
            Generator.new @source_d, "this_directory_must_not_exist"
        end
    end
    def test_prob_dir_exists
        prob = Problem.new
        prob.name = "JustATest"
        prob_d = File.join @target_d, prob.name
        FileUtils.mkdir prob_d unless File.exists? prob_d
        assert_raise ProblemDirExists do @generator.generate prob end
        FileUtils.rmdir prob_d
    end
end