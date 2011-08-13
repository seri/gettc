package org.topcoder;

public class ParseException extends TopcoderException {
    public ParseException() {
        super("ParseException occurred for an unknown cause");
    }
    public ParseException(String msg) {
        super(msg);
    }
    public ParseException(String msg, String context) {
        super(msg + " at: <" + context + ">");
    }
    public ParseException(char expected, char actual, String context) {
        super("Expect '" + expected + "' at: <" + context + "> " +
              "(got '" + actual + "')");
    }
}
