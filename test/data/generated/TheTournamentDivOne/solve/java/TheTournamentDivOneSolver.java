import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class TheTournamentDivOneSolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        List<Integer> pointsBoxed = (List<Integer>) reader.next(new TypeRef<List<Integer>>(){}.getType());
        int[] points = new int[pointsBoxed.size()];
        for (int _i = 0; _i < pointsBoxed.size(); ++_i)
            points[_i] = pointsBoxed.get(_i);
        reader.next();
        
        int w = (Integer) reader.next(Integer.class);
        reader.next();
        
        int d = (Integer) reader.next(Integer.class);
        reader.close();

        TheTournamentDivOne solver = new TheTournamentDivOne();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        writer.write(solver.find(points, w, d));
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
