Draw Console
------------

Run Instructions
----------------
1. CD to DrawConsole folder
2. To run tests
- sbt test
3. To run program:
- sbt run
4. Alternatively right-click on Boot.scala and select Run
5. Follow on screen instructions
6. Available Commands and format are available in the Code Challenge PDF file. 

Design Notes & Future Enhancements
----------------------------------
- All unit-tests are passing

- The program works for all commands requested in the Spec and many edge cases are also handled.

- The task instructions do not say to handle lower case commands, so commands must be upper case Char

- CommandListener singleton is currently viewed as boilerplate code that don't have a unit-test as its output to Console is a side-effect and it just runs in a recursive fashion returning Unit. 
Since time is limited for this task, i would consider revisiting this singleton in the future. 
Maybe System.setIn(in) and System.setOut(out) could be used in conjunction with dependency injection to allow different in/out streams to be used for execution or tests.   

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