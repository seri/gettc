require "fileutils"
require "logger"
require "pathname"

class SysUtil
    def self.with_dir dir
        saved = FileUtils.pwd
        FileUtils.chdir dir
        yield
        FileUtils.chdir saved
    end
end

class StopWatch
    def self.measure
        before = Time.now
        yield
        after = Time.now
        return ((after - before) * 1000).round
    end
end

class GettcRunner
    def initialize mode, solver, data_d, output_d
        @verbose = (mode == "verbose")
        @solver = solver
        @data_d = data_d
        @output_d = output_d

        @dir = File.dirname __FILE__

        FileUtils.mkdir_p @output_d

        @log = Logger.new STDOUT
        @log.formatter = proc do |severity, datetime, progname, message|
            message
        end
        @log.level = Logger::INFO
        if @verbose
            @log.level = Logger::DEBUG
        end 
    end

    def compile_checker
        puts "[gettc] Compile checker"
        @checker = File.join @dir, "../build/check"
        unless File.exists? @checker
            checker_src = File.join @dir, "../util/check"
            SysUtil::with_dir checker_src do 
                system "make"
            end
        end
    end

    def populate_filenames n
        input = File.join @data_d, "#{n}.in"
        expected = File.join @data_d, "#{n}.out"
        received = File.join @output_d, "#{n}.out"
        return input, expected, received
    end

    def print_file_content label, filename
        print "    #{label}: <"
        print File.read(filename)
        puts ">"
    end

    def print_case n
        input, expected, received = populate_filenames n
        print_file_content "Input", input
        print_file_content "Expected", expected
        print_file_content "Received", received
    end

    def run_case n
        input, expected, received = populate_filenames n

        @log.debug "Case #{n} ... "
        ret = true

        elapsed = StopWatch::measure do
            ret = system "#{@solver} #{input} #{received}"
        end
        @elapsed[n] = elapsed
        @log.debug "#{elapsed}ms "

        if ret != true
            @errors << n
            @log.debug "Error (cannot execute solver)\n"
        elsif !File.exists? received
            @errors << n
            @log.debug "Error (solver did not produce an output file)\n"
        else
            system "#{@checker} #{expected} #{received}"
            ret = $?.exitstatus
            case ret
            when 0
                @log.debug "Passed\n"
            when 1
                @log.debug "Failed\n"
                @failures << n
                print_case n if @verbose
            when 2
                @log.debug "Error (checker reported error)\n"
                @errors << n
            else
                @log.debug "Error (error executing checker)\n"
                @errors << n
            end
        end 
    end

    def run_all_cases
        puts "[gettc] Run test cases"

        inputs = Dir.glob "#{@data_d}/*.in"
        inputs.sort!

        @total = inputs.size
        @elapsed = Hash.new
        @failures = Array.new
        @errors = Array.new

        inputs.each do |input|
            basename = Pathname.new(input).basename.to_s
            run_case basename[0..-4]
        end
    end

    def print_array label, arr
        if arr.size > 0
            print "#{label}: "
            puts arr.join(", ")
        end 
    end

    def summarize
        puts "[gettc] Summary"
        puts "#{@total} cases checked, #{@failures.size} failures, #{@errors.size} errors"
        print_array "Failures", @failures
        print_array "Errors", @errors

        if @total > 0
            sum = 0
            maxv = 0
            maxk = nil
            @elapsed.each do |key, value|
                sum += value
                if value > maxv
                    maxv = value
                    maxk = key
                end 
            end
            avg = sum / @total

            puts "    Total time taken: #{sum} ms"
            puts "    Average time taken: #{avg} ms"
            puts "    Slowest running case: #{maxv} ms (case #{maxk})"
        end 
    end

    def print_failures
        if @failures.empty? || @verbose
            return
        end 
        n =  @failures.size
        if n > 5
            puts "Here are a few failed case for your debugging purposes"
            n = 5
        end
        n.times do |i|
            j = @failures[i]
            puts "Case #{j}:"
            print_case j
        end
    end

    def run
        compile_checker
        run_all_cases
        print_failures
        summarize
    end
end

runner = GettcRunner.new ARGV[0], ARGV[1], ARGV[2], ARGV[3]
runner.run
