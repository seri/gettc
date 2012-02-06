require 'rake/testtask' 
require 'rubygems/package_task' 
require 'rubygems/installer' 
require 'fileutils' 

task :default => [:install] 

Rake::TestTask.new do |t|
    t.libs   << 'core/lib'
    t.pattern = 'core/test/**/*_test.rb'
end

spec = Gem::Specification.new do |s| 
    s.platform = Gem::Platform::RUBY
    s.name = 'gettc'
    s.summary = 'Download TopCoder problem and generate a skeleton solution'
    s.description = 'Given a TopCoder problem ID, gettc downloads the problem specification, parses the whole thing into a Markdown file, generates inputs/outputs based on the Examples and System Tests given, and finally generates basic solution files for you to get started.'
    s.version = '1.1.1'

    s.author = 'Seri'
    s.email = 'seritrinh@gmail.com'
    s.homepage = 'http://seriessays.blogspot.com'

    s.files  = FileList["{bin,dist,core/lib}/**/*"].to_a + ['Rakefile']
    s.test_files = FileList["core/test/**/*_test.rb"].to_a
    s.require_path = 'core/lib'
    s.has_rdoc = false

    s.bindir = 'bin'
    s.executables = ['gettc']

    s.add_dependency 'hpricot'
    s.add_dependency 'bluecloth'
end 
Gem::PackageTask.new spec do |pkg|
    pkg.need_tar = true
end

task :clean do
    if File.exists? 'pkg' then
        FileUtils.rm_rf 'pkg'
    end
end 

task :install do
    `rake repackage --quiet`

    wizard = Gem::Installer.new "pkg/#{spec.file_name}"
    begin
        wizard.install
    rescue Gem::FilePermissionError
        `sudo gem i pkg/#{spec.file_name}`
    rescue StandardError => err
        puts err
        exit -1
    end

    config_d = File.expand_path '~/.gettc'
    if File.exists? config_d then
        puts "#{config_d} exists ..."
        print 'Do you want to remove it for a clean upgrade? (yn) '
        if STDIN.gets.strip.capitalize.start_with? 'Y' then 
            FileUtils.rm_rf config_d
        end
    end 

    puts 'Successfully installed. Run gettc <id> to get started.'
end
