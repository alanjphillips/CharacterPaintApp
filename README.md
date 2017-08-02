Draw Console featuring immutable ScanLine Floodfill implementation
------------------------------------------------------------------
Floodfill implementation definitely needs refactoring but as a starting point it works.

Run Instructions
----------------
1. CD to DrawConsole folder
2. To run tests
- sbt test
3. To run program:
- sbt run

BucketFill functionality
------------------------
- BucketFill functionality is based on the following algorithm, Queue based ScanLine Fill:
https://en.wikipedia.org/wiki/Flood_fill#Scanline_fill

From Wikipedia page:

Flood-fill (node, target-color, replacement-color):
 1. If target-color is equal to replacement-color, return.
 2. If color of node is not equal to target-color, return.
 3. Set Q to the empty queue.
 4. Add node to Q.
 5. For each element N of Q:
 6.         Set w and e equal to N.
 7.         Move w to the west until the color of the node to the west of w no longer matches target-color.
 8.         Move e to the east until the color of the node to the east of e no longer matches target-color.
 9.         For each node n between w and e:
10.             Set the color of n to replacement-color.
11.             If the color of the node to the north of n is target-color, add that node to Q.
12.             If the color of the node to the south of n is target-color, add that node to Q.
13. Continue looping until Q is exhausted.
14. Return.

Advantages:
- Tail recursive so stack overflow problems are avoided
- Immutable design