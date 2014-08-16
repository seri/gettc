require "test/unit"
require "gettc/signature" 
require "engine" 
include Gettc

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
            vector<double> numbers; tc::read(ifs, numbers); tc::next(ifs);
            string name; tc::read(ifs, name); tc::next(ifs);
            int pivot; tc::read(ifs, pivot); tc::next(ifs);
            bool rounded; tc::read(ifs, rounded);
        eos
        assert_equal result.rstrip, @engine.input
    end
end
