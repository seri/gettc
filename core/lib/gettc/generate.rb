require "fileutils"
require "pathname"
require "erb" 

require "gettc/problem"
require "gettc/signature" 
require "gettc/print"

module Gettc
  GenerateError = Class.new StandardError
  class ProblemDirExists < GenerateError
    attr_accessor :dir
    def initialize dir, msg = nil
      if msg.nil? then
        msg = "Cannot create problem directory because it already exists"
      end 
      @dir = dir
      super "#{msg} (#{dir})"
    end
  end
  class SourceDirNotExist < GenerateError
    attr_accessor :dir
    def initialize dir, msg = "Source directory does not exist"
      @dir = dir
      super "#{msg} (#{dir})"
    end
  end
  class TargetDirNotExist < GenerateError
    attr_accessor :dir
    def initialize dir, msg = "Target directory does not exist"
      @dir = dir
      super "#{msg} (#{dir})"
    end
  end
  class TemplateError < GenerateError
    attr_accessor :dir
    def initialize err, source, msg = "Template error"
      @err = err
      @source = source
      super "#{msg} (#{source})\n#{err}"
    end
  end 
  class Generator
    def initialize config_d, target_d
      @source_d = File.join config_d, "template"
      @target_d = target_d
      raise SourceDirNotExist.new @source_d unless File.directory? @source_d 
      raise TargetDirNotExist.new @target_d unless File.directory? @target_d

      include_d = File.join config_d, "include"
      load_engines include_d
    end
    def generate prob
      @prob = prob
      @prob_d = File.join @target_d, prob.name
      raise ProblemDirExists.new @prob_d if File.exists? @prob_d
      FileUtils.mkdir @prob_d

      method_sig = @prob.definitions["Method signature"]
      if method_sig.nil? then 
        $stderr.puts "[Warning] No definition for method signature found"
      else
        vars = parse_method_signature method_sig
        func = vars.shift
      end
      @context = binding

      walk @source_d, @prob_d
    end
  private      
    def gen_images images, images_d
      images.each do |image|
        filename = File.join images_d, image.name
        File.open filename, "wb" do |f| f.write image.content end
      end
    end
    def gen_cases cases, data_d
      cases.each_index do |i|
        c = cases[i]
        File.open File.join(data_d, "#{i.to_s}.in"), "w" do |f| 
          f.write c.input 
        end 
        File.open File.join(data_d, "#{i.to_s}.out"), "w" do |f| 
          f.write c.output
        end 
      end 
    end
    def gen_template source, target
      before = File.open source, "r" do |f| f.read end
      begin
        after = ERB.new(before).result @context
      rescue SyntaxError, NameError => err
        raise TemplateError.new err, source
      end
      File.open target, "w" do |f| f.write after end
    end
    def filter target_d, name
      if name == "{images_d}" then
        gen_images @prob.images, target_d
      elsif name == "{examples_d}" then
        gen_cases @prob.examples, target_d
      elsif name == "{systests_d}" then
        gen_cases @prob.systests, target_d
      else
        target_n = name.gsub /\{(\w*)\}/ do |match|
          @prob.name if $1 == "name"
        end 
        return target_n
      end
      return nil
    end
    def load_engines include_d
      return unless File.exists? include_d
      Dir.foreach include_d do |name|
        child = File.join include_d, name
        if File.directory? child then
          engine = File.join child, "engine.rb"
          if File.exists? engine then
            unless (Pathname.new engine).absolute? then
              engine = "./" + engine              
            end
            require engine
          end
        end
      end
    end
    def walk source_d, target_d
      Dir.foreach source_d do |name|
        if name != "." and name != ".." then
          source_p = File.join source_d, name
          target_n = filter target_d, name
          unless target_n.nil? then
            target_p = File.join target_d, target_n
            if File.directory? source_p then
              FileUtils.mkdir target_p unless File.exists? target_p
              walk source_p, target_p
            elsif File.file? source_p then
              gen_template source_p, target_p
            end
          end 
        end 
      end 
    end
  end
end
