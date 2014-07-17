require "test/unit"
require "topcoder/signature" 
require "engine" 
include TopCoder

class CppEngineTest < Test::Unit::TestCase
    def setup
        func = Signature.new TArray.new(TDouble), "getMaxMin"
        vars = [ Signature.new(TArray.new(TDouble), "numbers"),
                 Signature.new(TString, "name"),
                 Signature.new(TInt, "pivot"),
                 Signature.new(TBoolean, "rounded") ]
        @engine = CppEngine.new func, vars
    end
    def test_declare
        result = <<-eos.gsub(/^ {12}/, "")
            vector<double> getMaxMin(vector<double> const &numbers,
                                     string const &name,
                                     int pivot,
                                     bool rounded)
        eos
        assert_equal result.rstrip, @engine.declare
    end
    def test_input
        result = <<-eos.gsub(/^ {12}/, "")
            vector<double> numbers; read(ifs, numbers); next(ifs);
            string name; read(ifs, name); next(ifs);
            int pivot; read(ifs, pivot); next(ifs);
            bool rounded; read(ifs, rounded);
        eos
        assert_equal result.rstrip, @engine.input
    end
end
