#define BOOST_TEST_DYN_LINK 
#define BOOST_TEST_MODULE MyTest
#include <boost/test/unit_test.hpp> 
#include "<%= prob.name %>.cpp" 

BOOST_AUTO_TEST_CASE (test_<%= func.name %>) {
    BOOST_CHECK_EQUAL(0, 0);
}
