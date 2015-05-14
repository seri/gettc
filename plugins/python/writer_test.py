import os, sys
import unittest
from helper import deep_equals

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.join(script_dir, "dist/include/python"))
import topcoder as tc

class WriterTest(unittest.TestCase):
    def setUp(self):
        pass

    def check(self, value, type):
        self.assertTrue(deep_equals(tc.Reader(tc.write(value, type)).next(type), value))

    def test_write(self):
        self.check(20.14, "double")
        self.check('@', "char")
        self.check('String white a "', "String")
        self.check([1, 2, 3, 4], "int[]")
        self.check([[20.14]], "double[][]")
        self.check([[[]]], "boolean[][][]")
        self.check(False, "boolean")
        self.check([["Jon Snow"],['The "Little Finger"']], "String[][]")
        with self.assertRaises(tc.UnsupportedType):
            tc.write({}, "dict")

if __name__ == "__main__":
    unittest.main()
