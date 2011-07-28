require 'fileutils'
require 'erb' 

require 'topcoder/problem'
require 'topcoder/signature' 
require 'topcoder/print'

module TopCoder
    class GenerateError < StandardError
    end
    class ProblemDirExists < GenerateError
        attr_accessor :dir
        def initialize dir, msg = nil
            if msg.nil? then
                msg = 'Cannot create problem directory because it already exists'
            end 
            @dir = dir
            super "#{msg} (#{dir})"
        end
    end
    class SourceDirNotExist < GenerateError
        attr_accessor :dir
        def initialize dir, msg = nil
            if msg.nil? then
                msg = 'Source directory does not exist'
            end 
            @dir = dir
            super "#{msg} (#{dir})"
        end
    end
    class TargetDirNotExist < GenerateError
        attr_accessor :dir
        def initialize dir, msg = nil
            if msg.nil? then
                msg = 'Target directory does not exist'
            end 
            @dir = dir
            super "#{msg} (#{dir})"
        end
    end
    class ProblemDirExists < GenerateError
        attr_accessor :dir
        def initialize dir, msg
            if msg.nil? then
                msg = 'Target directory does not exist'
            end 
            @dir = dir
            super "#{msg} (#{dir})"
        end
    end
    class Generator
        def initialize source_d, target_d
            raise SourceDirNotExist.new source_d if not File.directory? source_d 
            raise TargetDirNotExist.new target_d if not File.directory? target_d
            @source_d = source_d
            @target_d = target_d
        end
        def gen_images images, images_d
            images.each do |image|
                filename = File.join images_d, image.name
                File.open filename, 'wb' do |f| f.write image.content end
            end
        end
        def gen_cases cases, data_d
            cases.each_index do |i|
                c = cases[i]
                File.open File.join(data_d, "#{i.to_s}.in"), 'w' do |f| 
                    f.write c.input 
                end 
                File.open File.join(data_d, "#{i.to_s}.out"), 'w' do |f| 
                    f.write c.output
                end 
            end 
        end
        def gen_template source, target
            before = File.open source, 'r' do |f| 
                f.read 
            end
            begin
                after = ERB.new(before).result @context
            rescue StandardError => err
                puts "Template error (#{File.expand_path source}): "
                puts err.backtrace.join "\n"
            end
            File.open target, 'w' do |f| f.write after end
        end
        def filter target_d, name
            if name == '{images_d}' then
                gen_images @prob.images, target_d
            elsif name == '{examples_d}' then
                gen_cases @prob.examples, target_d
            elsif name == '{systests_d}' then
                gen_cases @prob.systests, target_d
            else
                target_n = name.gsub /{(\w*)}/ do |match|
                    case $1
                    when 'name' then
                        @prob.name
                    end
                end 
                return target_n
            end
            return nil
        end
        def walk source_d, target_d
            Dir.foreach source_d do |name|
                if name != '.' and name != '..' then
                    source_p = File.join source_d, name
                    target_n = filter target_d, name
                    if not target_n.nil? then
                        target_p = File.join target_d, target_n
                        if File.directory? source_p then
                            FileUtils.mkdir target_p if not File.exists? target_p
                            walk source_p, target_p
                        elsif File.file? source_p then
                            gen_template source_p, target_p
                        end
                    end 
                end 
            end 
        end
        def generate prob
            @prob = prob
            @prob_d = File.join @target_d, prob.name
            if File.exists? @prob_d
                raise ProblemDirExists.new nil, @prob_d
            end
            FileUtils.mkdir @prob_d

            method_sig = @prob.definitions['Method signature']
            if method_sig.nil? then 
                $stderr.puts '[Warning] No definition for method signature found'
            else
                vars = parse_method_signature method_sig
                func = vars.shift
            end
            @context = binding 

            walk @source_d, @prob_d
        end
    end
end
