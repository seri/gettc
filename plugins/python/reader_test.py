import os, sys
import unittest
from helper import deep_equals

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.join(script_dir, "dist/include/python"))
import topcoder as tc

class ReaderTest(unittest.TestCase):
    def setUp(self):
        pass

    def test_deep_equals(self):
        self.assertTrue(deep_equals([], []))
        self.assertTrue(deep_equals([123], [123]))
        self.assertFalse(deep_equals([123], [123, 456]))
        self.assertTrue(deep_equals([[123], [1, 2, 3]], [[123], [1, 2, 3]]))

    def test_unsupported_type(self):
        reader = tc.Reader("whatever")
        with self.assertRaises(tc.UnsupportedTypeError):
            reader.next("object")


    def test_read_boolean(self):
        reader = tc.Reader("true")
        flag = reader.next("boolean")
        self.assertEqual(flag, True)
        reader = tc.Reader("false")
        flag = reader.next("boolean")
        self.assertEqual(flag, False)

    def test_read_boolean_error(self):
        reader = tc.Reader("lol")
        with self.assertRaises(tc.ReadError):
            flag = reader.next("boolean")

    def test_read_positive_int(self):
        reader = tc.Reader("20.14")
        number = reader.next("int")
        self.assertEqual(number, 20)

    def test_read_negative_int(self):
        reader = tc.Reader("-1")
        number = reader.next("int")
        self.assertEqual(number, -1)

    def test_read_int_error(self):
        reader = tc.Reader("NaN")
        with self.assertRaises(tc.ReadError):
            number = reader.next("int")

    def test_read_long(self):
        reader = tc.Reader("1234567890123456789")
        number = reader.next("long")
        self.assertEqual(number, 1234567890123456789)

    def test_read_positive_float(self):
        reader = tc.Reader("20.14")
        number = reader.next("float")
        self.assertEqual(number, 20.14)

    def test_read_negative_float(self):
        reader = tc.Reader("-2")
        number = reader.next("float")
        self.assertEqual(number, -2)


    def test_read_quoted_char(self):
        reader = tc.Reader("'@'")
        character = reader.next("char")
        self.assertEqual(character, '@')

    def test_read_unquoted_char(self):
        reader = tc.Reader("@")
        character = reader.next("char")
        self.assertEqual(character, '@')

    def test_read_char_error(self):
        reader = tc.Reader("'@ '")
        with self.assertRaises(tc.ReadError):
            number = reader.next("char")


    def test_read_string(self):
        reader = tc.Reader('"Eddard Stark"')
        name = reader.next("String")
        self.assertEqual(name, "Eddard Stark")

    def test_read_string_error(self):
        reader = tc.Reader('"Eddard Stark')
        with self.assertRaises(tc.ReadError):
            name = reader.next("String")
        reader = tc.Reader('Eddard Stark"')
        with self.assertRaises(tc.ReadError):
            name = reader.next("String")

    def test_read_string_with_quote(self):
        reader = tc.Reader('"Eddard "Ned"", "Stark"')
        name = reader.next("String")
        self.assertEqual(name, 'Eddard "Ned"')


    def test_read_empty_array (self):
        reader = tc.Reader("[]")
        arr = reader.next("int[]")

    def test_read_array(self):
        reader = tc.Reader("[2, 3, 1, 4, 6, 5]")
        arr = reader.next("int[]")
        self.assertTrue(deep_equals(arr, [2, 3, 1, 4, 6, 5]))

    def test_read_array_2d(self):
        reader = tc.Reader('[ ["abc", "def"], ["foo", "bar"] ]')
        arr = reader.next("String[][]")
        self.assertTrue(deep_equals(arr, [["abc", "def"], ["foo", "bar"]]))


    def test_read_everything(self):
        text = """
            [ [ "Jon Snow"
              , "Lord Varys"
              , "The "Little Finger"" ]
            , [ ] ]
            , C, 20.14, fAlSe, 'X'
            , [ -2 , 0 , 1 , 4 ]
        """
        reader = tc.Reader(text)
        self.assertTrue(deep_equals(reader.next("String[][]"), 
                        [["Jon Snow", "Lord Varys", 'The "Little Finger"'], []]))
        reader.next()
        self.assertEqual(reader.next("char"), 'C')
        reader.next()
        self.assertEqual(reader.next("double"), 20.14)
        reader.next()
        self.assertEqual(reader.next("boolean"), False)
        reader.next()
        self.assertEqual(reader.next("char"), 'X')
        reader.next()
        self.assertEqual(reader.next("int[]"), [-2, 0, 1, 4])

if __name__ == "__main__":
    unittest.main()
