#! /usr/bin/python3

import os, sys
import CirclesCountry

def init():
    default_path = os.path.join(os.getenv("HOME"), ".gettc")
    gettc_home = os.path.abspath(os.getenv("GETTC_HOME", default_path))
    include_dir = os.path.join(gettc_home, "include/python")
    sys.path.append(include_dir)

def main():
    import topcoder as tc
    with open(sys.argv[1], "r") as fi:
        input = fi.read()
        reader = tc.Reader(input)

        X = reader.next("int[]")
        reader.next()
        Y = reader.next("int[]")
        reader.next()
        R = reader.next("int[]")
        reader.next()
        x1 = reader.next("int")
        reader.next()
        y1 = reader.next("int")
        reader.next()
        x2 = reader.next("int")
        reader.next()
        y2 = reader.next("int")

    result = CirclesCountry.leastBorders(X, Y, R, x1, y1, x2, y2)
    with open(sys.argv[2], "w") as fo:
        fo.write(tc.write(result, "int"))

if __name__ == "__main__":
    init()
    main()
