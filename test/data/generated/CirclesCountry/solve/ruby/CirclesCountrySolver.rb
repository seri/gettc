#! /usr/bin/env ruby

require_relative "CirclesCountry"

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
  X = reader.next(TArray.new(TInt))
  reader.next()
  Y = reader.next(TArray.new(TInt))
  reader.next()
  R = reader.next(TArray.new(TInt))
  reader.next()
  x1 = reader.next(TInt)
  reader.next()
  y1 = reader.next(TInt)
  reader.next()
  x2 = reader.next(TInt)
  reader.next()
  y2 = reader.next(TInt)

  result = CirclesCountry.new().leastBorders(X, Y, R, x1, y1, x2, y2)
  IO.write(ARGV[1], Writer.new.next(result, TInt).to_s)
end

init
main
