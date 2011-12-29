require 'test/unit'
require 'topcoder/signature' 
require 'engine'
include TopCoder

class HaskellEngineTest < Test::Unit::TestCase
    def setup
        func = Signature.new TArray.new(TDouble), 'GetMaxMin'
        vars = [ Signature.new(TArray.new(TArray.new(TInt)), 'Numbers'),
                 Signature.new(TString, 'name'),
                 Signature.new(TInt, 'pivot'),
                 Signature.new(TBoolean, 'rounded') ]
        @engine = HaskellEngine.new func, vars
    end
    def test_declare
        result = <<-eos.gsub(/^ {12}/, '')
            getMaxMin :: [[Int]] -> String -> Int -> Bool -> [Double]
            getMaxMin numbers name pivot rounded = []
        eos
        assert_equal result.rstrip, @engine.declare
    end
    def test_input
        result = <<-eos.gsub(/^ {12}/,'')
            getVars :: Parser ([[Int]], String, Int, Bool)
            getVars = do numbers <- spaces >> (parseList (parseList parseInt)) ; next
                         name <- spaces >> parseString ; next
                         pivot <- spaces >> parseInt ; next
                         rounded <- spaces >> parseBool
                         return (numbers, name, pivot, rounded)
        eos
        assert_equal result.rstrip, @engine.input
    end
    def test_output
        result = "getMaxMin numbers name pivot rounded"
        assert_equal result, @engine.output
    end
end
