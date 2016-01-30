package main

import (
    "./PageNumbers"
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
        N int
    )

    if fileHandle, errIO = os.Open(os.Args[1]); errIO != nil {
        panic(errIO)
    }
    topcoder.Read(fileHandle, &N)
    fileHandle.Close()

    if fileHandle, errIO = os.Create(os.Args[2]); errIO != nil {
        panic(errIO)
    }
    topcoder.Write(fileHandle, PageNumbers.GetCounts(N))
}
