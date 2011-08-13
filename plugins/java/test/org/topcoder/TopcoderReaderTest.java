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

        source("123.456");
        double d = (Double) reader.next(Double.class);
        assertEquals(123.456, d, EPSILON);
        
        source("'c'");
        char c = (Character) reader.next(Character.class);
        assertEquals('c', c);

        source("\"Hello World\"");
        String s = (String) reader.next(String.class);
        assertEquals("Hello World", s);
    }

    @Test public void arrays() throws IOException {
        source("[]");
        List<Integer> a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertTrue(a.isEmpty());

        source("[123,234,5]");
        a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertEquals(3,   a.size());
        assertEquals(123, (int) a.get(0));
        assertEquals(234, (int) a.get(1));
        assertEquals(5,   (int) a.get(2));

        source("  [  123  ,\n234,     5]");
        a = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        assertEquals(3,   a.size());
        assertEquals(123, (int) a.get(0));
        assertEquals(234, (int) a.get(1));
        assertEquals(5,   (int) a.get(2));

        source("[[\"Hello\", \"World\"], [\"With\", \"Java\"]]");
        List<List<String>> aa = (List<List<String>>) reader.next(new TypeRef<List<List<String>>>(){}.getType());
        assertEquals(2, aa.size());
        assertEquals("World", aa.get(0).get(1));
        assertEquals("With", aa.get(1).get(0));
    }

    @Test public void realLife() throws IOException {
        source("\"Seri\", 'M', 24, [\"Hello\",\n \"Good World!!!\",\n \"\"]");
        String name = (String) reader.next(String.class); reader.next();
        char gender = (Character) reader.next(Character.class); reader.next();
        int age = (Integer) reader.next(Integer.class); reader.next();
        List<String> textsBoxed = (List<String>) reader.next(new TypeRef<List<String>>(){}.getType());

        String[] texts = new String[textsBoxed.size()];
        for (int _i = 0; _i != texts.length; ++_i)
            texts[_i] = textsBoxed.get(_i);

        assertEquals("Seri", name);
        assertEquals('M', gender);
        assertEquals(24, age);
        assertEquals(3, texts.length);
        assertEquals("Hello", texts[0]);
        assertEquals("Good World!!!", texts[1]);
        assertTrue(texts[2].isEmpty());

        source("[\"We have the best mortgage rates. Refinance today.\",\"Money-making opportunity! $5000/week potential!!!\",\"Don't Feel Short; try Elevator Shoes for increase.\",\n \"All-new pics: Stacy, Tiffany, Donner, and Blitzen.\"],\n[\"5000 bucks for shoes?\",\n \"Short bucks for new shoes?\"]");
        
        List<String> knownSpam = (List<String>) reader.next(new TypeRef<List<String>>(){}.getType());
        for (String elem : knownSpam) {
            System.out.println(elem);
        }
        reader.next();
        List<String> mails = (List<String>) reader.next(new TypeRef<List<String>>(){}.getType());
        for (String elem : mails) {
            System.out.println(elem);
        }
    }
}