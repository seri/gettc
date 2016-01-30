#! /usr/bin/env ruby

require_relative "BackyardTrees"

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
  treeCount = reader.next(TInt)
  reader.next()
  width = reader.next(TInt)
  reader.next()
  height = reader.next(TInt)
  reader.next()
  distance = reader.next(TInt)

  result = BackyardTrees.new().countWays(treeCount, width, height, distance)
  IO.write(ARGV[1], Writer.new.next(result, TInt).to_s)
end

init
main
