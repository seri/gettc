# [BuildingRoads](/tc?module=ProblemDetail&rd=14153&pm=10737)
*Single Round Match 470 Round 1 - Division I, Level Three*

## Statement
You are given the map of a country which is divided into a rectangular grid of cells. The country contains several cities, each of which occupies a single cell. Your goal is to connect certain pairs of cities with direct roads. However, there are several large rocks which might get in your way. Each rock potentially spans multiple cells, and costs some amount of money to destroy. You want to build your roads while spending as little money as possible destroying rocks.

You are given the map in the String[] *field*. The j-th character of the i-th element of field represents the cell at row i, column j. Each cell is one of the following:
'.' - Empty space.
'!', '@', '#', or '$' - A city. For each of these characters, there can be either 0 or 2 cities denoted by it.
'a'-'z', 'A'-'Z', or '0'-'9' - A cell which is occupied entirely by a rock.
Cells containing rocks which are denoted by the same character are occupied by the same physical rock. Each pair of cells which is occupied by the same rock will be connected. This means that if you label the cells C1 and C2, at least one of the following will be true:
C1 and C2 share a side.
There exists another cell C3, where C1 and C3 share a side, and C3 is connected to C2.
The character used to denote a rock represents the cost required to destroy that entire rock.  The costs are as follows:
'a' - 'z': 1 - 26
'A' - 'Z': 100 - 2,600 (each letter costs 100 more than the previous letter)
'1' - '9': 10,000 - 90,000 (each digit costs 10,000 more than the previous digit)
'0': 100,000
You must build a road between each pair of cities denoted by the same character. Each road must be a continuous (but not necessarily straight) line which starts in the middle of one city and ends in the middle of the other city. Roads cannot be built outside the boundaries of the map, and roads must not touch any cell which contains a rock (not even the sides or corners of such cells). Return the minimum cost required to destroy all the rocks necessary to build the roads.

## Definitions
- *Class*: `BuildingRoads`
- *Method*: `destroyRocks`
- *Parameters*: `String[]`
- *Returns*: `int`
- *Method signature*: `int destroyRocks(String[] field)`

## Notes
- The roads are assumed to be arbitrarily thin, and can intersect each other.
- The roads may pass through any city.

## Constraints
- *field* will contain between 2 and 50 elements, inclusive.
- Each element of *field* will contain between 1 and 50 characters, inclusive.
- Each element of *field* will contain the same number of characters.
- Each character in *field* will be '.', 'a'-'z', 'A'-'Z', '0'-'9', '!', '@', '#', or '$'.
- *field* will contain either zero or two occurrences of '!'.
- *field* will contain either zero or two occurrences of '@'.
- *field* will contain either zero or two occurrences of '#'.
- *field* will contain either zero or two occurrences of '$'.
- There will be at least two cities in *field*.
- Each pair of cells which is occupied by the same rock will be connected (as described in the problem statement).

## Examples
### Example 1
#### Input
<c>["!1.!",<br /> "aab2"]</c>
#### Output
<c>3</c>
#### Reason
![image](images/buildingroads1.png)

You must destroy the two rocks which cover the purple cells. The total cost is 3.

### Example 2
#### Input
<c>["#@",<br /> "A.",<br /> "A1",<br /> "@#"]</c>
#### Output
<c>100</c>
#### Reason
![image](images/buildingroads2.png)

### Example 3
#### Input
<c>["$....",<br /> "BBBBB",<br /> "B000B",<br /> "B0$0B",<br /> "B000B",<br /> "BBBBB"]</c>
#### Output
<c>100200</c>
### Example 4
#### Input
<c>["$a",<br /> ".B",<br /> "$3"]</c>
#### Output
<c>0</c>
### Example 5
#### Input
<c>[".#!@.$",<br /> ".11111",<br /> "..AB..",<br /> "33AB..",<br /> "$3AB..",<br /> "88888a",<br /> "#!@..."]</c>
#### Output
<c>30301</c>

