require 'test/unit'
require 'topcoder/signature' 
require 'engine' 
include TopCoder

class JavaEngineTest < Test::Unit::TestCase
    def setup
        func = Signature.new TArray.new(TDouble), 'getMaxMin'
        vars = [ Signature.new(TArray.new(TArray.new(TInt)), 'numbers'),
                 Signature.new(TString, 'name'),
                 Signature.new(TInt, 'pivot') ]
        @engine = JavaEngine.new func, vars
    end
    def test_declare
        result = <<-eos.gsub(/^ {12}/, '')
            double[] getMaxMin(int[][] numbers,
                               String name,
                               int pivot)
        eos
        assert_equal result.rstrip, @engine.declare
    end
    def test_array_depth
        assert_equal 1, JavaArray.get_depth(@engine.func.type)        
        assert_equal 2, JavaArray.get_depth(TArray.new(TArray.new(TString)))
    end
    def test_input
        result = <<-eos
List<List<Integer>> numbersBoxed = (List<List<Integer>>) reader.next(new TypeRef<List<List<Integer>>>(){}.getType());
int[][] numbers = new int[numbersBoxed.size()][numbersBoxed.get(0).size()];
for (int _i = 0; _i < numbersBoxed.size(); ++_i)
    for (int _j = 0; _j < numbersBoxed.get(0).size(); ++_j)
        numbers[_i][_j] = numbersBoxed.get(_i).get(_j);
reader.next();

String name = (String) reader.next(String.class);
reader.next();

int pivot = (Integer) reader.next(Integer.class);
        eos
        assert_equal result.rstrip, @engine.input
    end
    def test_box_double_array
        sig = Signature.new TArray.new(TArray.new(TString)), 'matrix'
        arr = JavaArray.new sig
        result = <<-eos
List<List<String>> matrixBoxed = new ArrayList<List<String>>();
for (int _i = 0; _i < matrix.length; ++_i) {
    List<String> matrixBoxed_i = new ArrayList<String>();
    for (int _j = 0; _j < matrix[_i].length; ++_j) {
        matrixBoxed_i.add(matrix[_i][_j]);
    }
    matrixBoxed.add(matrixBoxed_i);
}
        eos
        assert_equal result, arr.box_code
    end
    def test_output_array
        result = <<-eos
double[] result = solver.getMaxMin(numbers, name, pivot);
List<Double> resultBoxed = new ArrayList<Double>();
for (int _i = 0; _i < result.length; ++_i) {
    resultBoxed.add(result[_i]);
}
writer.write(resultBoxed);
        eos
        assert_equal result.rstrip, @engine.output
    end
    def test_output_prim
        func = Signature.new TInt, 'count'
        vars = []
        engine = JavaEngine.new func, vars
        result = "writer.write(solver.count());"
        assert_equal result, engine.output
    end
end
