package org.topcoder;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

// Based on an idea by Neal Gafter
// Seaerch for "super type tokens" if you're curious
public abstract class TypeRef<T> {
    private final Type type;
    protected TypeRef() {
        type = ((ParameterizedType) getClass().getGenericSuperclass())
                                              .getActualTypeArguments()[0];
    }
    @Override public boolean equals (Object o) {
        return o instanceof TypeRef &&
            ((TypeRef)o).type.equals(type);
    }
    @Override public int hashCode() {
        return type.hashCode();
    }
    public Type getType() {
        return type;
    }
}
