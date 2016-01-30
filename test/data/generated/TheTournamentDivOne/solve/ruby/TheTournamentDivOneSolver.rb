#! /usr/bin/env ruby

require_relative "TheTournamentDivOne"

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
  points = reader.next(TArray.new(TInt))
  reader.next()
  w = reader.next(TInt)
  reader.next()
  d = reader.next(TInt)

  result = TheTournamentDivOne.new().find(points, w, d)
  IO.write(ARGV[1], Writer.new.next(result, TInt).to_s)
end

init
main
