package topcoder

import (
    "bytes"
    "bufio"
    "fmt"
    "io"
    "reflect"
    "unicode"
)

func spaces(br *bufio.Reader) error {
    for {
        switch b, err := br.ReadByte(); {
        case err == io.EOF:
            return nil
        case err != nil:
            return err
        case !unicode.IsSpace(rune(b)):
            br.UnreadByte()
            return nil
        }
    }
    return nil
}

func genericExpect(br *bufio.Reader, what string, predicate func(byte) bool) error {
    switch b, err := br.ReadByte(); {
    case err == io.EOF:
        return io.ErrUnexpectedEOF
    case err != nil:
        return err
    case !predicate(b):
        br.UnreadByte()
        return NewErrExpect(br, what)
    }
    return nil
}

func expect(br *bufio.Reader, b byte) error {
    return genericExpect(br, fmt.Sprintf("<%c>", b), func(got byte) bool {
        return got == b
    })
}

func next(br *bufio.Reader) error {
    if err := spaces(br); err != nil {
        return err
    }
    return expect(br, ',')
}


func readPrimitive(br *bufio.Reader, x interface{}) error {
    _, err := fmt.Fscan(br, x)
    return err
}

func readChar(br *bufio.Reader, pChar *byte) error {
    var (
        err error
        b byte
    )
    if err = spaces(br); err != nil {
        return err
    }
    if err = expect(br, '\''); err != nil {
        return err
    }
    switch b, err = br.ReadByte(); {
    case err == io.EOF:
        return io.ErrUnexpectedEOF
    case err != nil:
        return err
    }
    if err = expect(br, '\''); err != nil {
        return err
    }
    *pChar = b
    return nil
}


func isEndOfString(br *bufio.Reader) (ended bool, text string, err error) {
    var b byte
    w := bytes.NewBufferString("")
    for {
        switch b, err = br.ReadByte(); {
        case err == io.EOF:
            ended = true
            err = nil
            return
        case err != nil:
            return
        case b == ',' || b == ']':
            br.UnreadByte()
            ended = true
            return
        case !unicode.IsSpace(rune(b)):
            ended = false
            text = w.String()
            err = nil
            br.UnreadByte()
            return
        }
        w.WriteByte(b)
    }
    return
}

func readString(br *bufio.Reader, pString *string) error {
    var (
        b byte
        err error
        ended bool
        text string
    )
    if err = spaces(br); err != nil {
        return err
    }
    if err = expect(br, '"'); err != nil {
        return err
    }
    w := bytes.NewBufferString("")
    for {
        switch b, err = br.ReadByte(); {
        case err == io.EOF:
            return io.ErrUnexpectedEOF
        case err != nil:
            return err
        case b != '"':
            if err = w.WriteByte(b); err != nil {
                return err
            }
            continue
        }
        switch ended, text, err = isEndOfString(br); {
        case err != nil:
            return err
        case ended:
            *pString = w.String()
            return nil
        }
        w.WriteByte('"')
        w.WriteString(text)
    }
    return nil
}


func isEndOfSlice(br *bufio.Reader) (bool, error) {
    var (
        b byte
        err error
    )
    if err = spaces(br); err != nil {
        return false, err
    }
    switch b, err = br.ReadByte(); {
    case err != nil:
        return false, err
    case b == ']':
        return true, nil
    }
    br.UnreadByte()
    return false, nil
}

func betterNew(typ reflect.Type) reflect.Value {
    ret := reflect.New(typ)
    if typ.Kind() == reflect.Slice {
        slice := reflect.MakeSlice(typ, 0, 0)
        reflect.Indirect(ret).Set(slice)
    }
    return ret
}

func appendSlice(br *bufio.Reader, slice reflect.Value) error {
    another := betterNew(slice.Type().Elem())
    if err := readOne(br, another.Interface()); err != nil {
        return err
    }
    slice.Set(reflect.Append(slice, another.Elem()))
    return nil
}

func readSlice(br *bufio.Reader, x interface{}) error {
    var (
        err error
        ended bool
    )

    value := reflect.ValueOf(x)
    slice := value.Elem()
    if slice.Kind() != reflect.Slice {
        return NewErrType(value.Type())
    }

    if err = spaces(br); err != nil {
        return err
    }
    if err = expect(br, '['); err != nil {
        return err
    }

    switch ended, err = isEndOfSlice(br); {
    case err != nil:
        return err
    case ended:
        return nil
    }
    if err = appendSlice(br, slice); err != nil {
        return err
    }

    for {
        switch ended, err = isEndOfSlice(br); {
        case err != nil:
            return err
        case ended:
            return nil
        }
        if err = next(br); err != nil {
            return err
        }
        if err = appendSlice(br, slice); err != nil {
            return err
        }
    }

	return nil
}


func readOne(br *bufio.Reader, x interface{}) error {
    switch p := x.(type) {
    case *bool, *int, *int64, *float32, *float64:
        return readPrimitive(br, x)
    case *byte:
        return readChar(br, p)
    case *string:
        return readString(br, p)
    default:
        value := reflect.ValueOf(x)
        if value.Kind() != reflect.Ptr {
            return NewErrType(value.Type())
        }
        return readSlice(br, x)
    }
    return nil
}

func Read(r io.Reader, xs ...interface{}) error {
    br := bufio.NewReader(r)
    for i, x := range xs {
        if i != 0 {
            if err := next(br); err != nil {
                return err
            }
        }
        if err := readOne(br, x); err != nil {
            return err
        }
    }
    return nil
}
