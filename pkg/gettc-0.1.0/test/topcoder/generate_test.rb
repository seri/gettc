require 'test/unit' 
require 'topcoder/generate' 
require 'fileutils'
require 'tmpdir'
include TopCoder

class GenerateTest < Test::Unit::TestCase
    def setup
        @srcdir = File.join File.dirname(__FILE__), '../../template'
        @tmpdir = File.join Dir.tmpdir, 'gettc-test'
        if not File.directory? @tmpdir
            Dir.mkdir @tmpdir            
        end
    end
    def test_problem_dir_exists
        prob = Problem.new
        prob.name = 'JustATest'
        probdir = File.join @tmpdir, prob.name
        if not File.exists? probdir
            FileUtils.mkdir probdir
        end
        assert_raise ProblemDirExists do
            generator = Generator.new @srcdir, @tmpdir, prob, []   
            generator.generate
        end
        FileUtils.rmdir probdir
    end
end
