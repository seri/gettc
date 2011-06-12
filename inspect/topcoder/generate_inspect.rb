require 'tmpdir' 
require 'fileutils' 

require 'topcoder/generate'
include TopCoder

srcdir = File.join File.dirname(__FILE__), '../../template'
tardir = File.join Dir.tmpdir, 'gettc-test'
if not File.exists? tardir then
    FileUtils.mkdir tardir
end 

prob = Problem.new
prob.name = 'CirclesCountry'
prob.url = 'http://www.topcoder.com/tc?module=ProblemDetail&rd=13751&pm=10297'
prob.source = 'Single Round Match 443 Round 1 - Division I, Level One'
prob.statement = <<-END
Circles Country is a country that contains several circular-shaped districts. Some districts may be situated inside other districts, but their borders do not intersect or touch. Qatam is a resident of Circles Country. When he travels between two locations, he always tries to cross the fewest number of district borders as possible because crossing borders is usually a laborious task.
Imagine Circles Country as an infinite plane. You are given int[]s X, Y and R, where (X[i], Y[i]) are the coordinates of the i-th district's center and R[i] is its radius. Qatam is currently at point (x1,y1) and he needs to get to point (x2,y2). Neither of these points lies on a district border. Return the minimal number of district borders he must cross to get to his destination.
END
prob.definitions = {                    
    'Class' => 'CirclesCountry',
    'Method' => 'leastBorders',
    'Parameters' => 'int[], int[], int[], int, int, int, int',
    'Returns' => 'int',
    'Method signature' => 'int[] funnyCalculation(String name, String[] addresses, double[] grades, char gender, float[] heights, float[] weights, long[][] matrix)'
}
prob.notes = []
prob.constraints = [
    'X will contain between 1 and 50 elements, inclusive.',
    'X, Y and R will each contain the same number of elements.',
    'Each element of X and Y will be between -1000 and 1000, inclusive.',
    'Each element of R will be between 1 and 1000, inclusive.',
    'x1, y1, x2 and y2 will be between -1000 and 1000, inclusive.',
    'No two circumferences will have common points.',
    'The points (x1,y1) and (x2,y2) will not lie on any of the circumferences.'
]
example = Example.new
example.input = "[0]\n[0]\n[2]\n-5\n1\n5\n1"
example.output = '0'
example.reason = '![image](images/case1.gif)'
prob.examples = [example]

images = [
    'http://www.topcoder.com/contest/problem/CirclesCountry/case1.gif',
    'http://www.topcoder.com/contest/problem/CirclesCountry/case2.gif',
    'http://www.topcoder.com/contest/problem/CirclesCountry/case3.gif',
    'http://www.topcoder.com/contest/problem/CirclesCountry/case4.gif'
]

generator = Generator.new srcdir, tardir, prob, images
generator.generate
