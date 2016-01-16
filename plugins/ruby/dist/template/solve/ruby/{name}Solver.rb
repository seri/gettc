#! /usr/bin/env ruby
<% engine = RubyEngine.new(func, vars) %>
require_relative "<%= prob.name %>"

require "gettc/types"
include Gettc

def init
  gettc_home = File.expand_path(ENV["GETTC_HOME"] || File.join(ENV["HOME"], ".gettc"))
  $LOAD_PATH << File.join(gettc_home, "include", "ruby")
  require "topcoder"
  include TopCoder
end

def main
  reader = Reader.new(IO.read(ARGV[0]))
<%= engine.input.gsub(/^/, " " * 2) %>

  result = <%= prob.name %>.new().<%= func.name %>(<%= engine.arglist %>)
  IO.write(ARGV[1], Writer.new.next(result, <%= func.type.to_ruby %>).to_s)
end

init
main
