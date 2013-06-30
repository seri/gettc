require 'rake/testtask' 
require 'rubygems/package_task' 
require 'rubygems/installer' 
require 'fileutils' 

Rake::TestTask.new do |t|
    t.libs   << 'core/lib'
    t.pattern = 'core/test/**/*_test.rb'
end

spec = Gem::Specification.new do |s| 
    s.platform = Gem::Platform::RUBY
    s.name = 'gettc'
    s.summary = 'Download TopCoder problem and generate a skeleton solution'
    s.description = 'Given a TopCoder problem ID, gettc downloads the problem specification, parses the whole thing into a Markdown file, generates inputs/outputs based on the Examples and System Tests given, and finally generates basic solution files for you to get started.'
    s.version = '1.2.2'

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
