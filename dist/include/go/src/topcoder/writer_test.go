package topcoder

import (
    "bytes"
    "testing"
)

func writer() *bytes.Buffer {
    return bytes.NewBufferString("")
}

func TestShow(t *testing.T) {
    out := writer()
    Write(out, "Joe", 20, 7.5, false, []string{"Math", "CompSci"}, [][]int{{7, 8}, {}})
    ret := out.String()
    if ret != "\"Joe\", 20, 7.500000, false, [\"Math\", \"CompSci\"], [[7, 8], []]" {
        t.Errorf("Write() produces <" + ret + ">")
    }
}
