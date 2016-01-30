#include <gtest/gtest.h>
#include <topcoder>
using namespace TopCoder;
using namespace std;

TEST(IsSameTest, Numbers) {
  EXPECT_TRUE(same(1, 1));
  EXPECT_TRUE(same(0.1234567890, 0.1234567891));
  EXPECT_FALSE(same(0.123456789, 0.123456890));
}

TEST(IsSameTest, EmptyArrays) {
  vector<int> v1;
  vector<int> v2;
  EXPECT_TRUE(same(v1, v2));
}

TEST(IsSameTest, OneDimensionalArrays) {
  int a1[] = {1, 3, 5, 4, 2};
  vector<int> v1(a1, a1 + 5);
  int a2[] = {1, 3, 5, 4, 2};
  vector<int> v2(a2, a2 + 5);
  EXPECT_TRUE(same(v1, v2));
  v2.push_back(6);
  EXPECT_FALSE(same(v1, v2));
}

TEST(IsSameTest, TwoDimensionalArrays) {
  vector<string> v1e1;
  v1e1.push_back("Jon Snow");
  v1e1.push_back("Grey Worm");
  v1e1.push_back("Tywin Lannister");
  vector< vector<string> > v1;
  v1.push_back(v1e1);

  vector<string> v2e1;
  v2e1.push_back("Jon Snow");
  v2e1.push_back("Grey Worm");
  v2e1.push_back("Tywin Lannister");
  vector< vector<string> > v2;
  v2.push_back(v2e1);

  EXPECT_TRUE(same(v1, v2));
}

TEST(ReadTest, StringMissingClosingQuote) {
  istringstream iss("\"I'm not gonna close the quote, kay?");
  string name; EXPECT_THROW(read(iss, name), ParseException);
}

TEST(ReadTest, NonBooleanValue) {
  istringstream iss("This is not a boolean value");
  bool cool; EXPECT_THROW(read(iss, cool), ParseException);
}

TEST(ReadTest, ReadEmptyArray) {
  istringstream iss("[]");
  vector<int> vec; read(iss, vec);
  EXPECT_EQ(0, vec.size());
}

TEST(ReadTest, ReadPositiveInt) {
  istringstream iss("2014");
  double grade; read(iss, grade);
  EXPECT_EQ(2014, grade);
}

TEST(ReadTest, ReadNegativeDouble) {
  istringstream iss("-123.04");
  double grade; read(iss, grade);
  EXPECT_EQ(-123.04, grade);
}

TEST(ReadTest, ReadQuotedCharacter) {
  istringstream iss("'B'");
  char grade; read(iss, grade);
  EXPECT_EQ('B', grade);
}

TEST(ReadTest, ReadUnquotedCharacter) {
  istringstream iss("C");
  char grade; read(iss, grade);
  EXPECT_EQ('C', grade);
}

TEST(ReadTest, ReadStringEasy) {
  istringstream iss("\"The Red Wedding\"    ");
  string horror; read(iss, horror);
  EXPECT_EQ("The Red Wedding", horror);
}

TEST(ReadTest, ReadStringWithQuotesGreedy) {
  istringstream iss("\"Are you talking about \"The Red Wedding\" ?\", Lord Varys?");
  string horror; read(iss, horror);
  EXPECT_EQ("Are you talking about \"The Red Wedding\" ?", horror);
  EXPECT_EQ(", Lord Varys?", _rest(iss));
}

TEST(ReadTest, ReadStringWithQuotesThenComma) {
  istringstream iss("\"Are you talking about \", The Red Wedding");
  string horror; read(iss, horror);
  EXPECT_EQ("Are you talking about ", horror);
  EXPECT_EQ(", The Red Wedding", _rest(iss));
}

TEST(ReadTest, ReadEverything) {
  istringstream iss("1.23, True, \n[ \"The \"Red\" Wedding\",\n\"The Hound\" ],\n2014, false, ");

  double grade; read(iss, grade); next(iss);
  bool gender; read(iss, gender); next(iss);
  vector<string> names; read(iss, names); next(iss);
  int year; read(iss, year); next(iss);
  bool passed; read(iss, passed); next(iss);

  EXPECT_EQ(1.23, grade);
  EXPECT_EQ(true, gender);
  EXPECT_EQ(2014, year);
  EXPECT_EQ(2, names.size());
  EXPECT_EQ("The \"Red\" Wedding", names[0]);
  EXPECT_EQ("The Hound", names[1]);
  EXPECT_EQ(false, passed);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
