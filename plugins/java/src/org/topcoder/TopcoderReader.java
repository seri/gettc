package org.topcoder;

import java.io.Reader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public class TopcoderReader {

    static public final char ENDF = 0;

    private BufferedReader source;

    public TopcoderReader(Reader source) {
        this.source = new BufferedReader(source);
    }

    public void close() throws IOException {
        source.close();
    }

    private char token() throws IOException {
        source.mark(1);
        int i = source.read();
        return i == -1 ? ENDF : (char) i;
    }

    private void reset() throws IOException {
        source.reset();
    }

    private String rest() throws IOException {
        reset();
        StringBuffer buffer = new StringBuffer();
        for (char c = token(); c != ENDF; c = token()) { 
            buffer.append(c);
        }
        return buffer.toString();
    }

    private char nextChar() throws IOException {
        for (char c = token(); c != ENDF; c = token()) {
            if (!Character.isWhitespace(c)) {
                return c;
            }
        }
        return ENDF;
    }

    private void expect(char expected) throws IOException {
        char actual = nextChar();
        if (actual != expected) {
            throw new ParseException(expected, actual, rest());
        }
    }

    private void expectDigit() throws IOException {
        char c = nextChar();
        if (!Character.isDigit(c)) {
            throw new ParseException("Expect a digit", rest());
        }
        reset();
    }

    private String nextDigits() throws IOException {
        expectDigit();
        StringBuffer buffer = new StringBuffer();
        for (char c = token(); c != ENDF; c = token()) {
            if (Character.isDigit(c) || c == '.') {
                buffer.append(c);
            } else { 
                reset(); 
                break; 
            }
        }
        return buffer.toString();
    }

    private String nextNumber() throws IOException {
        char c = nextChar();
        if (c == '-') {
            return "-" + nextDigits();
        }
        reset();
        return nextDigits();
    }

    private Integer nextInteger() throws IOException {
        return Integer.parseInt(nextNumber());
    }

    private Long nextLong() throws IOException {
        return Long.parseLong(nextNumber());
    }

    private Float nextFloat() throws IOException {
        return Float.parseFloat(nextNumber());
    }
    
    private Double nextDouble() throws IOException {
        return Double.parseDouble(nextNumber());
    }
    
    private Character nextCharacter() throws IOException {
        expect('\''); char c = nextChar(); expect('\'');
        return c;
    }
    
    private String nextString() throws IOException {
        expect('"');
        StringBuffer buffer = new StringBuffer();
        for (char c = token(); c != ENDF; c = token()) {
            if (c == '"') {
                source.mark(65536);
                while (true) {
                    int i = source.read(); char cc = (char) i;
                    if (i == -1 || cc == ',' || cc == ']') {
                        source.reset();
                        return buffer.toString();
                    } else if (!Character.isWhitespace(cc)) {
                        buffer.append('"');
                        source.reset();
                        break;
                    }
                }                
            } else {
                buffer.append(c);
            }
        }
        throw new ParseException("Expect a closing quote before end of input", rest());
    }
    
    private Boolean nextBoolean() throws IOException {
        StringBuffer buffer = new StringBuffer();
        for (char c = nextChar(); c != ENDF; c = token()) {
            if (Character.isLetter(c)) 
                buffer.append(c);
            else {
                reset(); 
                break;
            }
        }
        String s = buffer.toString().toLowerCase();
        if (s.equals("true")) {
            return true;
        } else if (s.equals("false")) {
            return false;
        } else {
            throw new ParseException("Expect a boolean value (true or false, got " 
                                      + s + ")", rest());
        }
    }

    private Object nextArray(Type type) throws IOException {
        ArrayList list = new ArrayList();
        expect('['); 

        char c = nextChar(); 
        if (c == ']') {
            return list; 
        }
        reset();
        
        while (true) {
            list.add(next(type)); 
            c = nextChar(); 
            if (c == ']') {
                return list;
            } else if (c != ',') {
                throw new ParseException(',', c, rest());
            }
        }
    }

    public void next() throws IOException {
        expect(',');
    }

    public Object next(Type type) throws IOException {
        if (type.equals(Integer.class)) {
            return nextInteger();
        } else if (type.equals(Long.class)) {
            return nextLong();
        } else if (type.equals(Float.class)) {
            return nextFloat();
        } else if (type.equals(Double.class)) {
            return nextDouble();
        } else if (type.equals(Character.class)) {
            return nextCharacter();
        } else if (type.equals(String.class)) {
            return nextString();
        } else if (type.equals(Boolean.class)) {
            return nextBoolean();
        } else if (type instanceof ParameterizedType) {
            ParameterizedType temp = (ParameterizedType) type;
            if (temp.getRawType().equals(List.class)) {
                return nextArray(temp.getActualTypeArguments()[0]);
            }
        }
        throw new UnsupportedTypeException(type);
    }
}
