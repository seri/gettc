import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class PageNumbersSolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        int N = (Integer) reader.next(Integer.class);
        reader.close();

        PageNumbers solver = new PageNumbers();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        int[] result = solver.getCounts(N);
        List<Integer> resultBoxed = new ArrayList<Integer>();
        for (int _i = 0; _i < result.length; ++_i) {
            resultBoxed.add(result[_i]);
        }
        writer.write(resultBoxed);
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
