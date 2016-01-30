# [CirclesCountry](/tc?module=ProblemDetail&rd=13751&pm=10297)
*Single Round Match 443 Round 1 - Division I, Level One*

## Statement
Circles Country is a country that contains several circular-shaped districts.  Some districts may be situated inside other districts, but their borders do not intersect or touch.  Qatam is a resident of Circles Country.  When he travels between two locations, he always tries to cross the fewest number of district borders as possible because crossing borders is usually a laborious task.

Imagine Circles Country as an infinite plane.  You are given int[]s *X*, *Y* and *R*, where (*X*[i], *Y*[i]) are the coordinates of the i-th district's center and *R*[i] is its radius.  Qatam is currently at point (*x1*,*y1*) and he needs to get to point (*x2*,*y2*).  Neither of these points lies on a district border.  Return the minimal number of district borders he must cross to get to his destination.

## Definitions
- *Class*: `CirclesCountry`
- *Method*: `leastBorders`
- *Parameters*: `int[], int[], int[], int, int, int, int`
- *Returns*: `int`
- *Method signature*: `int leastBorders(int[] X, int[] Y, int[] R, int x1, int y1, int x2, int y2)`

## Constraints
- *X* will contain between 1 and 50 elements, inclusive.
- *X*, *Y* and *R* will each contain the same number of elements.
- Each element of *X* and *Y* will be between -1000 and 1000, inclusive.
- Each element of *R* will be between 1 and 1000, inclusive.
- *x1*, *y1*, *x2* and *y2* will be between -1000 and 1000, inclusive.
- No two circumferences will have common points.
- The points (*x1*,*y1*) and (*x2*,*y2*) will not lie on any of the circumferences.

## Examples
### Example 1
#### Input
<c></c>
#### Output
<c>0</c>
#### Reason
![image](images/case1.gif)

### Example 2
#### Input
<c></c>
#### Output
<c>2</c>
#### Reason
![image](images/case2.gif)

### Example 3
#### Input
<c></c>
#### Output
<c>3</c>
#### Reason
![image](images/case3.gif)

### Example 4
#### Input
<c></c>
#### Output
<c>5</c>
#### Reason
![image](images/case4.gif)

### Example 5
#### Input
<c></c>
#### Output
<c>3</c>

