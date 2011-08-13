package org.topcoder;

import java.lang.reflect.Type;

public class UnsupportedTypeException extends TopcoderException {
    public UnsupportedTypeException() {
        super("UnsupportedTypeException has occurred");   
    }
    public UnsupportedTypeException(String msg) {
        super(msg);
    }
    public UnsupportedTypeException(Type type) {
        super(type + " is not a valid TopCoder type");
    }
}