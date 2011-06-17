require 'fileutils'
require 'pathname' 
require 'erb' 
require 'net/http' 
require 'uri'

require 'topcoder/problem'
require 'topcoder/signature' 
require 'topcoder/print'

module TopCoder
    class GenerateError < StandardError
    end
    class ProblemDirExists < GenerateError
        attr_accessor :dir
        def initialize dir, msg
            if msg.nil? then
                msg = 'Cannot create problem directory because it already existed'
            end 
            @dir = dir
            super "#{msg} #{dir}"
        end
    end
    class Generator
        def initialize source_d, target_d, prob, images = []
            @prob = prob
            @name = prob.name
            @source_d = source_d
            @target_d = target_d
            @images = images
        end
        def gen_images
            return if @images.empty?
            imgdir = File.join @target_d, 'images'
            FileUtils.mkdir imgdir
            @images.each do |url|
                uri = URI.parse url
                filename = File.join imgdir, Pathname.new(url).basename
                Net::HTTP.start uri.host, uri.port do |http|
                    res = http.get uri.path
                    File.open filename, 'wb' do |f|
                        f.write res.body
                    end 
                end 
            end
        end
        def gen_test_cases
            indir = File.join @target_d, 'input'
            FileUtils.mkdir indir
            outdir = File.join @target_d, 'output'
            FileUtils.mkdir outdir
            @prob.examples.each_index do |i|
                e = @prob.examples[i]
                File.open "#{indir}/#{i}", 'w' do |f|
                    f.write e.input
                end 
                File.open "#{outdir}/#{i}", 'w' do |f|
                    f.write e.output
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
            File.open target, 'w' do |f| 
                f.write after
            end
        end
        def walk source_d, target_d
            Dir.foreach source_d do |name|
                if name != '.' and name != '..' then
                    source_path = File.join source_d, name
                    target_name = name.gsub '{name}', @prob.name
                    target_path = File.join target_d, target_name
                    if File.directory? source_path then
                        Dir.mkdir target_path
                        walk source_path, target_path
                    elsif File.file? source_path then
                        gen_template source_path, target_path
                    end
                end 
            end 
        end
        def generate 
            @target_d = File.join @target_d, @name
            if File.exists? @target_d
                raise ProblemDirExists.new nil, @target_d
            end
            FileUtils.mkdir @target_d 

            gen_images
            gen_test_cases 

            method_sig = @prob.definitions['Method signature']
            if method_sig.nil? then 
                $stderr.puts 'No definition for method signature found'
            else
                vars = parse_method_signature method_sig
                func = vars.shift
            end
            @context = binding 

            walk @source_d, @target_d
        end
    end
    def generate source_d, target_d, prob, images = []
        generator = Generator.new source_d, target_d, prob, images
        generator.generate
    end
end
