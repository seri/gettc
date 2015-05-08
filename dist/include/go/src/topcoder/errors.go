// I hope to migrate this codebase to this style soon, to avoid the error
// checking madness: http://blog.golang.org/errors-are-values. However, it
// will break compatibility so I'll wait for a timely chance.

package topcoder

import (
    "io"
    "io/ioutil"
    "fmt"
    "reflect"
)


type ErrType struct {
    typ reflect.Type
}

func (me ErrType) Error() string {
    return fmt.Sprintf("Cannot handle type %s", me.typ.String())
}

func NewErrType(typ reflect.Type) *ErrType {
    return &ErrType { typ }
}


func rest(r io.Reader) string {
    bs, err := ioutil.ReadAll(r)
    if err != nil {
        return err.Error()
    }
    return string(bs)
}


type ErrExpect string

func (me ErrExpect) Error() string {
    return string(me)
}

func NewErrExpect(r io.Reader, what string) ErrExpect {
    return ErrExpect(fmt.Sprintf("Expect %s at <%s>", what, rest(r)))
}
