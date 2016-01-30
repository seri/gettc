import java.io.FileReader;
import java.io.FileWriter;

import java.util.List;
import java.util.ArrayList;

import org.topcoder.TopcoderReader;
import org.topcoder.TopcoderWriter;
import org.topcoder.TypeRef;

public class BuildingRoadsSolver {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
        List<String> fieldBoxed = (List<String>) reader.next(new TypeRef<List<String>>(){}.getType());
        String[] field = new String[fieldBoxed.size()];
        for (int _i = 0; _i < fieldBoxed.size(); ++_i)
            field[_i] = fieldBoxed.get(_i);
        reader.close();

        BuildingRoads solver = new BuildingRoads();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
        writer.write(solver.destroyRocks(field));
        writer.close();
    } catch (Exception err) {
        err.printStackTrace(System.err);
    }
    }
}
