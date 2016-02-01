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
  def initialize(mode, solver, data_d, output_d)
    @verbose = (mode == "verbose")
    @solver = solver
    @data_d = data_d

    @output_d = output_d
    FileUtils.mkdir_p(@output_d)

    @dir = File.dirname(__FILE__)

    @log = Logger.new(STDOUT)
    @log.formatter = proc do |severity, datetime, progname, message|
      message
    end
    @log.level = @verbose ? Logger::DEBUG : Logger::INFO
  end

  def compile_checker
    puts "[gettc] Compile checker"
    @checker = File.join(@dir, "../build/check")

    unless File.exists?(@checker)
      SysUtil::with_dir(File.join(@dir, "../util/check")) { system "make "}
    end
  end

  def populate_filenames(case_id)
    input = File.join(@data_d, "#{case_id}.in")
    expected = File.join(@data_d, "#{case_id}.out")
    received = File.join(@output_d, "#{case_id}.out")
    return input, expected, received
  end

  def print_file_content(label, filename)
    print "    #{label}: <"
    print File.read(filename)
    puts ">"
  end

  def print_case(case_id)
    input, expected, received = populate_filenames(case_id)
    print_file_content("Input", input)
    print_file_content("Expected", expected)
    print_file_content("Received", received)
  end

  def run_case(case_id)
    input, expected, received = populate_filenames(case_id)

    @log.debug "Case #{case_id} ... "
    ret = true

    elapsed = StopWatch::measure do
      ret = system "#{@solver} #{input} #{received}"
    end

    @elapsed[case_id] = elapsed
    @log.debug "#{elapsed}ms "

    if ret != true
      @errors << case_id
      @log.debug "Error (cannot execute solver)\n"
    elsif !File.exists?(received)
      @errors << case_id
      @log.debug "Error (solver did not produce an output file)\n"
    else
      system "#{@checker} #{expected} #{received}"
      ret = $?.exitstatus
      case ret
      when 0
        @log.debug "Passed\n"
      when 1
        @log.debug "Failed\n"
        @failures << case_id
        print_case(case_id) if @verbose
      when 2
        @log.debug "Error (checker reported error)\n"
        @errors << case_id
      else
        @log.debug "Error (error executing checker)\n"
        @errors << case_id
      end
    end
  end

  def run_all_cases
    puts "[gettc] Run test cases"

    inputs = Dir.glob("#{@data_d}/*.in").sort

    @total = inputs.size
    @elapsed = Hash.new
    @failures = Array.new
    @errors = Array.new

    inputs.each do |input|
      run_case(Pathname.new(input).basename.to_s[0..-4])
    end
  end

  def print_array(label, arr)
    unless arr.empty?
      print "#{label}: "
      puts arr.join(", ")
    end
  end

  def summarize
    puts "[gettc] Summary"
    puts "#{@total} cases checked, #{@failures.size} failures, #{@errors.size} errors"
    print_array("Failures", @failures)
    print_array("Errors", @errors)

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
    return if @failures.empty? || @verbose

    n = @failures.size
    if n > 5
      puts "Here are a few failed case for your debugging purposes"
      n = 5
    end

    n.times do |i|
      case_id = @failures[i]
      puts "Case #{case_id}:"
      print_case(case_id)
    end
  end

  def run
    compile_checker
    run_all_cases
    print_failures
    summarize
  end
end

GettcRunner.new(ARGV[0], ARGV[1], ARGV[2], ARGV[3]).run
