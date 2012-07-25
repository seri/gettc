package org.topcoder;

public class TopcoderException extends RuntimeException {
    public TopcoderException() {
        super("TopcoderException has occurred");   
    }
    public TopcoderException(String msg) {
        super(msg);
    }
}