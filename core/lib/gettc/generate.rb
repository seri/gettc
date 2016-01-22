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

    def initialize(dir, message = "Problem directory already exists")
      @dir = dir
      super "#{message}: (#{dir})"
    end
  end

  class SourceDirNotExist < GenerateError
    attr_accessor :dir

    def initialize(dir, message = "Source directory does not exist")
      @dir = dir
      super "#{message}: (#{dir})"
    end
  end

  class TargetDirNotExist < GenerateError
    attr_accessor :dir

    def initialize(dir, message = "Target directory does not exist")
      @dir = dir
      super "#{message}: (#{dir})"
    end
  end

  class TemplateError < GenerateError
    attr_accessor :dir

    def initialize(err, source, message = "Template error")
      @err = err
      @source = source
      super "#{message} (#{source})\n#{err}"
    end
  end

  class Generator
    def initialize(config_d, target_dir)
      @source_dir = File.join(config_d, "template")
      raise SourceDirNotExist.new(@source_dir) unless File.directory?(@source_dir)

      @target_dir = target_dir
      raise TargetDirNotExist.new(@target_dir) unless File.directory?(@target_dir)

      load_engines(File.join(config_d, "include"))
    end

    def generate(prob)
      @prob = prob

      @prob_dir = File.join(@target_dir, prob.name)
      raise ProblemDirExists.new(@prob_d) if File.exists?(@prob_dir)

      FileUtils.mkdir(@prob_dir)

      method_sig = @prob.definitions["Method signature"]
      if method_sig.nil?
        $stderr.puts "[Warning] No definition for method signature found"
      else
        vars = parse_method_signature(method_sig)
        func = vars.shift
      end
      @context = binding

      walk(@source_dir, @prob_dir)
    end

    private

    def generate_images(images, images_dir)
      images.each do |image|
        filename = File.join(images_dir, image.name)
        File.open(filename, "wb") { |f| f.write(image.content) }
      end
    end

    def generate_test_case(cases, data_dir)
      cases.each_with_index do |case_data, case_id|
        File.open(File.join(data_dir, "#{case_id.to_s}.in"), "w") { |f| f.write(case_data.input) }
        File.open(File.join(data_dir, "#{case_id.to_s}.out"), "w") { |f| f.write(case_data.output) }
      end
    end

    def generate_template(source, target)
      before = File.open(source, "r") { |f| f.read }

      begin
        after = ERB.new(before).result(@context)
      rescue SyntaxError, NameError => err
        raise TemplateError.new(err, source)
      end

      File.open(target, "w") { |f| f.write(after) }
    end

    def filter(target_dir, name)
      case name
      when "{images_dir}"
        generate_images(@prob.images, target_dir)
      when "{examples_d}"
        generate_test_case(@prob.examples, target_dir)
      when "{systests_d}"
        generate_test_case(@prob.systests, target_dir)
      else
        return name.gsub(/\{(\w*)\}/) { |match| @prob.name if $1 == "name" }
      end

      return nil
    end

    def load_engines(include_dir)
      return unless File.exists?(include_dir)

      Dir.foreach(include_dir) do |name|
        if File.directory?(child = File.join(include_dir, name))
          if File.exists?(engine = File.join(child, "engine.rb"))
            engine = "./#{engine}" unless Pathname.new(engine).absolute?
            require engine
          end
        end
      end
    end

    def walk(source_parent, target_parent)
      Dir.foreach(source_parent) do |name|
        next if [".", ".."].include?(name)

        target_name = filter(target_parent, name)
        next if target_name.nil?

        source_child = File.join(source_parent, name)
        target_child = File.join(target_parent, target_name)

        if File.directory?(source_child)
          FileUtils.mkdir(target_child) unless File.exists?(target_child)
          walk(source_child, target_child)
        elsif File.file?(source_child)
          generate_template(source_child, target_child)
        end
      end
    end
  end
end
