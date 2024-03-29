8 Bishops
=========

Quick'n'dirty solver for finding required moves to move 4 white and 4 black bishops from one side to another using valid chess moves:

 * board size is 5x4
 * bishops move diagonally
 * bishops can't move through existing bishops
 * a bishop should never be put to a position where it can be attacked by the enemy color

Starting position:

    B . . . W
    B . . . W
    B . . . W
    B . . . W

And one possible way to continue:
 
    B . . . W
    B . . W W
    B . . . .
    B . . . W


    B . . . W
    B B . W W
    . . . . .
    B . . . W

 
    B . . . W
    B B . W .
    . . . W .
    B . . . W


.. and so on.

Algorithm
---------

This solver is implemented with Haskell and is essentially breadth-first search (BFS) with a small twist: only boards that are completely unseen before are added. Previously seen boards are kept in a Set, which requires clumsy (and slow) feeling Ord instance for board.

I'm a bit puzzled that on top of the naive/initial pure BFS solution that Set is enough to have this thing run in sufficient time. This solver goes through 9842 boards in 0.6 s before finding a solution that requires 36 steps.

But hey, they say that perfect is the enemy of done (or good) :-)


Other
-----

 * Author: Heikki Salo
 * License: Public domain
