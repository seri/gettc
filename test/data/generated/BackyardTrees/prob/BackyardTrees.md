# [BackyardTrees](/tc?module=ProblemDetail&rd=10008&pm=6620)
*Single Round Match 328 Round 1 - Division I, Level Three*

## Statement
This problem statement contains images that may not display properly if viewed outside of the applet.

Your backyard is a rectangular grid that measures *width* x *height* square meters.  You would like to plant *treeCount* trees using the following rules: 

	- All trees must be planted at integer coordinates on the grid.

	- All trees must lie on the same straight line.

	- Each pair of trees must be at least *distance* meters away from each other.

For example, two (of many) ways in which four trees could be planted on a 10x10 grid if *distance* is 2 are depicted below:

![image](images/BackyardTrees1.png)   ![image](images/BackyardTrees2.png)

Return the number of distinct ways in which the trees could be planted, modulo 1,000,000,000.  Two layouts are considered distinct if there exists a point (x, y) such that one layout contains a tree at (x, y) and the other layout does not.

## Definitions
- *Class*: `BackyardTrees`
- *Method*: `countWays`
- *Parameters*: `int, int, int, int`
- *Returns*: `int`
- *Method signature*: `int countWays(int treeCount, int width, int height, int distance)`

## Constraints
- *treeCount* will be between 1 and 50, inclusive.
- *width* will be between 1 and 500, inclusive.
- *height* will be between 1 and 500, inclusive.
- *distance* will be between 1 and 50, inclusive.

## Examples
### Example 1
#### Input
<c></c>
#### Output
<c>300</c>
#### Reason
There are only two trees, and the distance between any two points with integer coordinates is always at least 1.  Therefore, you can place the two trees at any two points with integer coordinates.  There are 25 such points and this gives you 300 different ways to plant the trees.

### Example 2
#### Input
<c></c>
#### Output
<c>2</c>
#### Reason
The diagonal of the backyard has a length of 60, which is just enough to place 13 trees with a distance of 5 between each adjacent pair.  Luckily, these 13 points are at integer coordinates, so you can place the trees along either of the two diagonals while satisfying all the rules.

### Example 3
#### Input
<c></c>
#### Output
<c>88</c>
### Example 4
#### Input
<c></c>
#### Output
<c>102</c>
#### Reason
You can place the trees along the rows or the columns of the grid, as well as on the two diagonals.

### Example 5
#### Input
<c></c>
#### Output
<c>0</c>
#### Reason
You can't plant 6 trees on the same line with the necessary distance between them on this grid.

### Example 6
#### Input
<c></c>
#### Output
<c>490260662</c>

