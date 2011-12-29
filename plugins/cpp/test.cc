#define BOOST_TEST_DYN_LINK 
#define BOOST_TEST_MODULE TopCoderTest
#include <boost/test/unit_test.hpp>

#include "topcoder"
using namespace TopCoder; 
using namespace std; 

BOOST_AUTO_TEST_CASE(test_same)
{
    BOOST_CHECK(same(1, 1));
    BOOST_CHECK(same(0.1234567890, 0.1234567891));
    BOOST_CHECK(!same(0.123456789, 0.123456890));

    int a1[] = {1, 2, 3, 4, 5};
    vector<int> v1(a1, a1 + 5);
    int a2[] = {1, 2, 3, 4, 5};
    vector<int> v2(a2, a2 + 5);
    BOOST_CHECK(same(v1, v2));
    v2.push_back(6);
    BOOST_CHECK(!same(v1, v2));
}

BOOST_AUTO_TEST_CASE(test_read_error)
{
    istringstream iss; 
    iss.str("\"I'm not gonna close the quote, kay?"); 
    string name; BOOST_CHECK_THROW(read(iss, name), ParseException);
    iss.str("This is not a boolean value");
    bool cool; BOOST_CHECK_THROW(read(iss, cool), ParseException);
}

BOOST_AUTO_TEST_CASE(test_read_empty_array)
{
    istringstream iss;
    iss.str("[]");

    vector<int> vec; read(iss, vec);
    BOOST_CHECK_EQUAL(0, vec.size());
}

BOOST_AUTO_TEST_CASE(test_read_all)
{ 
    istringstream iss;
    iss.str("1.23, True, \n[ \"Seri\",\n\"Hello World\" ],\n2011, false, ");

    double grade; read(iss, grade); next(iss); 
    bool gender; read(iss, gender); next(iss);
    vector<string> names; read(iss, names); next(iss); 
    int year; read(iss, year); next(iss);
    bool passed; read(iss, passed); next(iss);

    BOOST_CHECK_EQUAL(1.23, grade);
    BOOST_CHECK_EQUAL(true, gender);
    BOOST_CHECK_EQUAL(2011, year);
    BOOST_CHECK_EQUAL(2, names.size());
    BOOST_CHECK_EQUAL("Seri", names[0]);
    BOOST_CHECK_EQUAL("Hello World", names[1]);
    BOOST_CHECK_EQUAL(false, passed);
}
