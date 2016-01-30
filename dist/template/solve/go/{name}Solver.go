package main

import (
    "./<%= prob.name %>"
    "fmt"
    "os"
    "topcoder"
)
<%
    engine = GoEngine.new(func, vars)
%>
func main() {
    if len(os.Args) < 3 {
        fmt.Fprintf(os.Stderr, "Usage: %s <input_file> <output_file>", os.Args[0])
        os.Exit(-1)
    }

    var (
        fileHandle *os.File
        errIO error
        <%= engine.declare.join("\n#{' ' * 8}") %>
    )

    if fileHandle, errIO = os.Open(os.Args[1]); errIO != nil {
        panic(errIO)
    }
    topcoder.Read(fileHandle, <%= engine.input %>)
    fileHandle.Close()

    if fileHandle, errIO = os.Create(os.Args[2]); errIO != nil {
        panic(errIO)
    }
    topcoder.Write(fileHandle, <%= prob.name %>.<%= engine.func_name %>(<%= engine.output %>))
}
