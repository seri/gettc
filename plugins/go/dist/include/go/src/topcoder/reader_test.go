package topcoder

import (
    "bufio"
    "fmt"
    "reflect"
    "strings"
    "testing"
)

func newReader(s string) *bufio.Reader {
    return bufio.NewReader(strings.NewReader(s))
}

func unexpectedError(err error) string {
    return " raised an unexpected Error: " + err.Error()
}

func expectError() string {
    return " should have raised an error"
}

func mismatch(got, required interface{}) string {
    return fmt.Sprintf(" got %v (expected %v instead)", got, required)
}

func TestReadBool(t *testing.T) {
    type aTest struct {
        in string
        out bool
        ok bool
    }
    var (
        xs = []aTest {
            aTest {"True", true, true},
            aTest {"\n\t\n   fAlSe", false, true},
            aTest {"truz", true, false},
        }
        got bool
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *bool)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadInt(t *testing.T) {
    type aTest struct {
        in string
        out int
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"123", 123, true},
            aTest{"\n\t\n   123", 123, true},
            aTest{"x123", 123, false},
        }
        got int
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *int)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadInt64(t *testing.T) {
    type aTest struct {
        in string
        out int64
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"1234567890123", 1234567890123, true},
            aTest{"\n\t\n   1234567890123", 1234567890123, true},
            aTest{"x123", 123, false},
        }
        got int64
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *int64)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadFloat64(t *testing.T) {
    type aTest struct {
        in string
        out float64
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"12345.6789", 12345.6789, true},
            aTest{"\n\t\n   12345.6789", 12345.6789, true},
            aTest{"x123", 123.0, false},
        }
        got float64
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *float64)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadByte(t *testing.T) {
    type aTest struct {
        in string
        out byte
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"'H'ello", 'H', true},
            aTest{"\n\t 'A'23", 'A', true},
            aTest{"Bello", 'B', true},
            aTest{"'C", 'C', false},
        }
        got byte
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *byte)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadString(t *testing.T) {
    type aTest struct {
        in string
        out string
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"\"H e l l o\"", "H e l l o", true},
            aTest{"\"Hello\"world", "Hello", false},
            aTest{"\"Hello\"    , world", "Hello", true},
            aTest{"\n\t \"Hello world\"!", "Hello world", false},
            aTest{"Hello", "Hello", false},
            aTest{"H\"ello\\\" worl\"d", "ello\" worl", false},
            aTest{"H\"ello\\\" worl\"d", "ello\" worl", false},
        }
        got string
    )
    for _, x := range xs {
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *string)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case got != x.out:
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadIntSlice(t *testing.T) {
    type aTest struct {
        in string
        out []int
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"[]", []int {}, true},
            aTest{"[1]", []int {1}, true},
            aTest{"[1, 2, 3]", []int {1, 2, 3}, true},
            aTest{"\n[\n1\t\n,2,3,1001\n\n]", []int {1, 2, 3, 1001}, true},
            aTest{"\n[\n1\t\n,2,3,1001\n\n", []int {1, 2, 3, 1001}, false},
        }
    )
    for _, x := range xs {
        got := []int {}
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *[]int)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case !reflect.DeepEqual(got, x.out):
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadStringSlice(t *testing.T) {
    type aTest struct {
        in string
        out []string
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"[]", []string {}, true},
            aTest{"[\"Hello\"]", []string {"Hello"}, true},
            aTest{"[\"Hello\", \"World\"]", []string {"Hello", "World"}, true},
            aTest{"[\"Hello \" World\"]", []string {"Hello \" World"}, true},
            aTest{"\n[\n\"Hello\"\t\n,\n\"Wo\",\"rld\"\n\n]", []string {"Hello", "Wo", "rld"}, true},
            aTest{"\n[\n\"Hello\"\t\n\n\n", []string {"Hello"}, false},
        }
    )
    for _, x := range xs {
        got := []string {}
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *[]string)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case !reflect.DeepEqual(got, x.out):
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadSliceOfIntSlice(t *testing.T) {
    type aTest struct {
        in string
        out [][]int
        ok bool
    }
    var (
        xs = []aTest {
            aTest{"[[]]", [][]int {{}}, true},
            aTest{"[[1],[2,3]]", [][]int {{1}, {2, 3}}, true},
            aTest{"\n\t[\n[1,2],\n[]\n,[3, 4, 5]]", [][]int {{1, 2}, {}, {3, 4, 5}}, true},
            aTest{"[[1], [2], [3]", [][]int {{}}, false},
            aTest{"[[7, 8], []]", [][]int {{7, 8}, {}}, true},
        }
    )
    for _, x := range xs {
        var got [][]int
        br := newReader(x.in)
        err := readOne(br, &got)
        f := fmt.Sprintf("readOne(%q, *[][]int)", x.in)
        switch {
        case x.ok && err != nil:
            t.Errorf(f + unexpectedError(err))
        case !x.ok && err == nil:
            t.Errorf(f + expectError())
        case !x.ok && err != nil:
            break
        case !reflect.DeepEqual(got, x.out):
            t.Errorf(f + mismatch(got, x.out))
        }
    }
}

func TestReadAll(t *testing.T) {
    var (
        name string
        age int
        gender byte
        average float32
		grade byte
        courses []string
        grades [][]float64
    )
    in := "\"Joe\", 20, 'M', 7.5, C, [\"Math\", \"Computer \"Science\"\"], [[7, 8], []]"
    br := newReader(in)
    f := fmt.Sprintf("Read(%v)", in)
    Read(br, &name, &age, &gender, &average, &grade, &courses, &grades)
    if name != "Joe" || age != 20 || average != 7.5 || gender != 'M' || grade != 'C' ||
        !reflect.DeepEqual(courses, []string{"Math", "Computer \"Science\""}) ||
        !reflect.DeepEqual(grades, [][]float64{{7, 8}, {}}) {
        t.Errorf(f + " produces (%q, %d, &q, %f, %v, %v)", name, age, gender, average, courses, grades)
    }
}
