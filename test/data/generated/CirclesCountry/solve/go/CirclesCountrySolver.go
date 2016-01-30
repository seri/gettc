package main

import (
    "./CirclesCountry"
    "fmt"
    "os"
    "topcoder"
)

func main() {
    if len(os.Args) < 3 {
        fmt.Fprintf(os.Stderr, "Usage: %s <input_file> <output_file>", os.Args[0])
        os.Exit(-1)
    }

    var (
        fileHandle *os.File
        errIO error
        X []int
        Y []int
        R []int
        x1 int
        y1 int
        x2 int
        y2 int
    )

    if fileHandle, errIO = os.Open(os.Args[1]); errIO != nil {
        panic(errIO)
    }
    topcoder.Read(fileHandle, &X, &Y, &R, &x1, &y1, &x2, &y2)
    fileHandle.Close()

    if fileHandle, errIO = os.Create(os.Args[2]); errIO != nil {
        panic(errIO)
    }
    topcoder.Write(fileHandle, CirclesCountry.LeastBorders(X, Y, R, x1, y1, x2, y2))
}
