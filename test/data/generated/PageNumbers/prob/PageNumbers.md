# [PageNumbers](/tc?module=ProblemDetail&rd=13757&pm=10329)
*2009 TCO Algorithm Qualification Round 2 - Division I, Level Two*

## Statement
We have a book with *N* pages, numbered 1 to *N*.
How many times does each digit occur in the page numbers?

You are given an int *N*. Return a int[] with 10 elements, where for all i between 0 and 9, inclusive, the element i will be the number of times digit i occurs when we write down all the numbers between 1 and *N*, inclusive.

## Definitions
- *Class*: `PageNumbers`
- *Method*: `getCounts`
- *Parameters*: `int`
- *Returns*: `int[]`
- *Method signature*: `int[] getCounts(int N)`

## Notes
- You may assume that for any valid input each of the output values fits into an int.

## Constraints
- *N* will be between 1 and 1,000,000,000, inclusive.

## Examples
### Example 1
#### Input
<c>7</c>
#### Output
<c>[0, 1, 1, 1, 1, 1, 1, 1, 0, 0 ]</c>
#### Reason
The page numbers in this case are simply 1, 2, 3, 4, 5, 6, and 7.

### Example 2
#### Input
<c>11</c>
#### Output
<c>[1, 4, 1, 1, 1, 1, 1, 1, 1, 1 ]</c>
#### Reason
In comparison to the previous case, we added the pages 8, 9, 10, and 11. Now we have each digit exactly once, except for the digit 1 that occurs four times: once in 1 and 10, and twice in 11.

### Example 3
#### Input
<c>19</c>
#### Output
<c>[1, 12, 2, 2, 2, 2, 2, 2, 2, 2 ]</c>
#### Reason
Digits 2 to 9 now occur twice each, and we have plenty of occurrences of the digit 1.

### Example 4
#### Input
<c>999</c>
#### Output
<c>[189, 300, 300, 300, 300, 300, 300, 300, 300, 300 ]</c>
#### Reason
Due to symmetry, each of the digits 1 to 9 occurs equally many times in the sequence 1,2,...,999.

### Example 5
#### Input
<c>543212345</c>
#### Output
<c>[429904664, 541008121, 540917467, 540117067, 533117017, 473117011, 429904664, 429904664, 429904664, 429904664 ]</c>
#### Reason
Watch out for the time limit.


