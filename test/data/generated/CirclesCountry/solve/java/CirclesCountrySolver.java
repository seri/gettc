import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class CirclesCountrySolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        List<Integer> XBoxed = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        int[] X = new int[XBoxed.size()];
        for (int _i = 0; _i < XBoxed.size(); ++_i)
            X[_i] = XBoxed.get(_i);
        reader.next();
        
        List<Integer> YBoxed = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        int[] Y = new int[YBoxed.size()];
        for (int _i = 0; _i < YBoxed.size(); ++_i)
            Y[_i] = YBoxed.get(_i);
        reader.next();
        
        List<Integer> RBoxed = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        int[] R = new int[RBoxed.size()];
        for (int _i = 0; _i < RBoxed.size(); ++_i)
            R[_i] = RBoxed.get(_i);
        reader.next();
        
        int x1 = (Integer) reader.next(Integer.class);
        reader.next();
        
        int y1 = (Integer) reader.next(Integer.class);
        reader.next();
        
        int x2 = (Integer) reader.next(Integer.class);
        reader.next();
        
        int y2 = (Integer) reader.next(Integer.class);
        reader.close();

        CirclesCountry solver = new CirclesCountry();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        writer.write(solver.leastBorders(X, Y, R, x1, y1, x2, y2));
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
