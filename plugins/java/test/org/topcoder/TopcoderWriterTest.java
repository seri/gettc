package org.topcoder;

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.io.StringWriter;
import java.io.IOException;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

public class TopcoderWriterTest {
    private TopcoderWriter writer;
    private StringWriter source;

    private String content() {
        return source.toString();
    }
    private void expect(String expected) {
        assertEquals(expected, content());
    }
    private <T> void write(T obj) throws IOException {
        writer.write(obj);
    }

    @Test(expected = UnsupportedTypeException.class) 
    public void writeBigInteger() throws IOException {
        write(new java.math.BigInteger("12345678901234567890"));
    }

    @Before public void setUp() {
        source = new StringWriter();
        writer = new TopcoderWriter(source);
    }
    @Test public void writeInt() throws IOException {
        write(5); expect("5");
    }
    @Test public void writeDouble() throws IOException {
        write(123.456); expect("123.456");
    }
    @Test public void writeString() throws IOException {
        write("Hello World"); expect("\"Hello World\"");
    }
    @Test public void writeChar() throws IOException {
        write('c'); expect("'c'");
    }
    @Test public void writeFalse() throws IOException {
        write(false); expect("false");
    }
    @Test public void writeTrue() throws IOException {
        write(true); expect("true");
    }
    @Test public void writeArray() throws IOException {
        List a = new ArrayList<Integer>();
        a.add(123); a.add(4); a.add(56789);
        write(a); expect("[123, 4, 56789]");
    }
    @Test public void writeDoubleArray() throws IOException {
        ArrayList<ArrayList<String>> aa = new ArrayList<ArrayList<String>>();
        ArrayList<String> a1 = new ArrayList<String>(); a1.add("Hello"); a1.add("World");
        ArrayList<String> a2 = new ArrayList<String>(); a2.add("Hi"); a2.add("TopCoder");
        aa.add(a1); aa.add(a2);
        write(aa); expect("[[\"Hello\", \"World\"], [\"Hi\", \"TopCoder\"]]");
    }
}
