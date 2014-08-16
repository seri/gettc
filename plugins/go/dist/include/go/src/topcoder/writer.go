package topcoder

import (
    "bufio"
    "fmt"
    "io"
    "reflect"
)

func writeOne(bw *bufio.Writer, x interface{}) error {
    switch y := x.(type) {
    case bool:
        fmt.Fprintf(bw, "%t", y)
    case byte:
        fmt.Fprintf(bw, "'%c'", y)
    case string:
        fmt.Fprintf(bw, "\"%s\"", y)
    case int, int64:
        fmt.Fprintf(bw, "%d", y)
    case float32, float64:
        fmt.Fprintf(bw, "%f", y)
    default:
        slice := reflect.ValueOf(x)
        if slice.Kind() != reflect.Slice {
            return NewErrType(slice.Type())
        }
        bw.WriteByte('[')
        for i := 0; i < slice.Len(); i++ {
            if i != 0 {
                bw.WriteString(", ")
            }
            elem := slice.Index(i)
            if err := writeOne(bw, elem.Interface()); err != nil {
                return err
            }
        }
        bw.WriteByte(']')
    }
    return nil
}

func Write(w io.Writer, xs ...interface{}) error {
    bw := bufio.NewWriter(w)
    for i, x := range xs {
        if i != 0 {
            bw.WriteString(", ")
        }
        if err := writeOne(bw, x); err != nil {
            return err
        }
    }
    bw.Flush()
    return nil
}