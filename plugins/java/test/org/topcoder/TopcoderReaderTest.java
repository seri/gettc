package org.topcoder;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.StringReader;
import java.io.IOException;
import java.util.List;

public class TopcoderReaderTest {
    private static double EPSILON = 1e-9;
    TopcoderReader reader;
    
    private void source(String src) {
        reader = new TopcoderReader(new StringReader(src));
    }

    @Test(expected = UnsupportedTypeException.class) 
    public void unsupportedType() throws IOException {
        source("1234567890123456890");
        reader.next(java.math.BigInteger.class);
    }

    @Test(expected = ParseException.class) 
    public void corruptedString() throws IOException {
        source("\"I forgot to close the quote");
        String s = (String) reader.next(String.class);
    }

    @Test(expected = ParseException.class) 
    public void corruptedNumber() throws IOException {
        source("x123");
        int i = (Integer) reader.next(Integer.class);
    }

    @Test(expected = ParseException.class) 
    public void corruptedBoolean() throws IOException {
        source("truz");
        boolean b = (Boolean) reader.next(Boolean.class);
    }

    @Test(expected = ParseException.class) 
    public void corruptedArray1() throws IOException {
        source("[1,2");
        List a = (List) reader.next(new TypeRef<List<Integer>>(){}.getType());
    }

    @Test(expected = ParseException.class) 
    public void corruptedArray2() throws IOException {
        source("[\"Hello\" xyz]");
        List a = (List) reader.next(new TypeRef<List<String>>(){}.getType());
    }

    @Test public void primitives() throws IOException {
        source("123");
        int i = (Integer) reader.next(Integer.class);
        assertEquals(123, i);

        source("-32");
        int j = (Integer) reader.next(Integer.class);
        assertEquals(-32, j);

        source("123.456");
        double d = (Double) reader.next(Double.class);
        assertEquals(123.456, d, EPSILON);
        
        source("'c'");
        char c = (Character) reader.next(Character.class);
        assertEquals('c', c);

        source("c");
        c = (Character) reader.next(Character.class);
        assertEquals('c', c);

        source("\"Hello World\"");
        String s = (String) reader.next(String.class);
        assertEquals("Hello World", s);

        source("  True  ");
        boolean b = (Boolean) reader.next(Boolean.class);
        assertEquals(true, b);
    }

    @Test public void stringWithQuotes() throws IOException {
        source("\"Welcome to \"Code Jam\"  !\"");
        String message = (String) reader.next(String.class);
        assertEquals("Welcome to \"Code Jam\"  !", message);
    }

    @Test(expected = ParseException.class) 
    public void corruptedStringWithQuotes() throws IOException {
        source("\"Welcome to \" No , or ] to end this nightmare?");
        String message = (String) reader.next(String.class);
    }

    @Test public void arrays() throws IOException {
        source("[]");
        List<Integer> a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertTrue(a.isEmpty());

        source("[123,-1,50]");
        a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertEquals(3,   a.size());
        assertEquals(123, (int) a.get(0));
        assertEquals(-1, (int) a.get(1));
        assertEquals(50,   (int) a.get(2));

        source("  [  123  ,\n234,     -5]");
        a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertEquals(3,   a.size());
        assertEquals(123, (int) a.get(0));
        assertEquals(234, (int) a.get(1));
        assertEquals(-5,   (int) a.get(2));

        source("[[\"Hello\", \"World\"], [\"With\", \"Java\"]]");
        List<List<String>> aa = (List<List<String>>) reader.next(new TypeRef<List<List<String>>>(){}.getType());
        assertEquals(2, aa.size());
        assertEquals("World", aa.get(0).get(1));
        assertEquals("With", aa.get(1).get(0));
    }

    @Test public void realLife() throws IOException {
        source("\"Seri\", 'M',\tfaLSe\t,99, C, [\"Welcome to \"Code Jam\" ?\",\n \"\",\n \"Hey!\"]");
        String name = (String) reader.next(String.class); reader.next();
        char gender = (Character) reader.next(Character.class); reader.next();
        boolean passed = (Boolean) reader.next(Boolean.class); reader.next();
        int age = (Integer) reader.next(Integer.class); reader.next();
        char grade = (Character) reader.next(Character.class); reader.next();
        List<String> textsBoxed = (List<String>) reader.next(new TypeRef<List<String>>(){}.getType());

        String[] texts = new String[textsBoxed.size()];
        for (int _i = 0; _i != texts.length; ++_i) {
            texts[_i] = textsBoxed.get(_i);
        }

        assertEquals("Seri", name);
        assertEquals('M', gender);
        assertEquals(false, passed);
        assertEquals(99, age);
        assertEquals('C', grade);
        assertEquals(3, texts.length);
        assertEquals("Welcome to \"Code Jam\" ?", texts[0]);
        assertEquals("", texts[1]);
        assertEquals("Hey!", texts[2]);
    }
}
