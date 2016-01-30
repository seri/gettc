# [TheTournamentDivOne](/tc?module=ProblemDetail&rd=13907&pm=10685)
*Single Round Match 453 Round 1 - Division I, Level Two*

## Statement
John and Brus have an interest in team sports tournaments.
They are currently investigating the intermediate standings of a tournament.
They don't know which games have been played so far.
They only know that the tournament is played using the following rules:

Each game is played between two teams and results in either a victory for one team or a draw.
If a team wins a game, it gains *w* points and its opponent gains no points.
In case of a draw, each team gains *d* points.
The score of a team is the sum of all the points it has gained from all its games.
There are no restrictions on which teams can play against each other, and each pair of teams can play against each other any number of times (possibly zero).

You are given a int[] *points* representing the intermediate standings of the tournament.
The i-th element of *points* is the score of the i-th team.
Return the minimum number of games that could have been played to achieve those standings.
If *points* doesn't represent a valid state, return -1 instead.

## Definitions
- *Class*: `TheTournamentDivOne`
- *Method*: `find`
- *Parameters*: `int[], int, int`
- *Returns*: `int`
- *Method signature*: `int find(int[] points, int w, int d)`

## Constraints
- *points* will contain between 2 and 50 elements, inclusive.
- Each element of *points* will be between 0 and 10,000, inclusive.
- *w* will be between 1 and 10,000, inclusive.
- *d* will be between 1 and *w*, inclusive.

## Examples
### Example 1
#### Input
<c>[10, 1, 1],<br />2,<br />1</c>
#### Output
<c>6</c>
#### Reason
The first team has five wins or four wins and two draws.
The second and the third teams each has one draw.

### Example 2
#### Input
<c>[1, 1, 1],<br />2,<br />1</c>
#### Output
<c>-1</c>
#### Reason
These standings are impossible.
For each of the three teams to have one point, each one must have played in a game that resulted in a draw.
There is no way for three teams to have each played in exactly one game.

### Example 3
#### Input
<c>[1, 4, 0, 2],<br />3,<br />1</c>
#### Output
<c>3</c>
#### Reason
The only win goes to the second team.

### Example 4
#### Input
<c>[8, 3, 8, 5, 9, 2, 7, 11],<br />3,<br />2</c>
#### Output
<c>15</c>

