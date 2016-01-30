import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class BackyardTreesSolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        int treeCount = (Integer) reader.next(Integer.class);
        reader.next();
        
        int width = (Integer) reader.next(Integer.class);
        reader.next();
        
        int height = (Integer) reader.next(Integer.class);
        reader.next();
        
        int distance = (Integer) reader.next(Integer.class);
        reader.close();

        BackyardTrees solver = new BackyardTrees();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        writer.write(solver.countWays(treeCount, width, height, distance));
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
