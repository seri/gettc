import Test.HUnit
import Solve
<% test_name = func.name[0].upcase + func.name[1..-1] %>
test<%= test_name %> :: Test
test<%= test_name %> = TestCase 
    (assertEqual "" 0 0)

tests = TestList [ TestLabel "test<%= test_name %>" test<%= test_name %> ]

main = runTestTT tests
