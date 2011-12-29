package org.topcoder;

import java.io.Writer;
import java.io.StringWriter;
import java.io.IOException;

import java.util.List;

public class TopcoderWriter {
    private Writer source;
    public TopcoderWriter(Writer source) {
        this.source = source;
    }
    public void close() throws IOException  {
        source.close();
    }
    public <T> void write(T obj) throws IOException {
        if (obj == null) {
            source.write('"');
            source.write('"');
            return;
        }
        Class type = obj.getClass();
        if (type.equals(Integer.class) || type.equals(Long.class)
                                       || type.equals(Float.class)
                                       || type.equals(Double.class)
                                       || type.equals(Boolean.class))
            source.write(obj.toString());            
        else if (type.equals(Character.class)) {
            source.write('\'');
            source.write(obj.toString());
            source.write('\'');
        }
        else if (type.equals(String.class)) {
            source.write('"');
            source.write(obj.toString());
            source.write('"');
        }
        else if (obj instanceof List) {
            source.write('[');
            List list = (List) obj;
            for (int i = 0; i < list.size() - 1; ++i) {
                write(list.get(i));
                source.write(", ");
            }
            if (!list.isEmpty()) write(list.get(list.size() - 1));
            source.write(']');
        }
        else throw new UnsupportedTypeException(obj.getClass());
    }
}
