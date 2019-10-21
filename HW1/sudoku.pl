/* ECS 140B Assignment 1 */
/* Sudoku Solver */

/* For this assignment you will need to implement your
own 9x9 Sudoku solver in SWI-Prolog.  Basically you
have a 9x9 grid, and you must fill in the
grid so that each row, each column, and each of 9 3x3
grids all contain the numbers 1 through 9. */

/* You will need to fill in the sudoku predicate below,
and also supply any helper predicates. You should think
about what has to be true to make a sudoku table valid
and work out how to check for each of these conditions. */

/* To test your program we will type "test." into
SWI-Prolog and study the results. We will also attempt
the further tests, 1, 2 and 3, if you have told us that
they will work. (These may take too long to compute.) */

/* When grading we will be looking for solutions that
work correctly, but we also want to see clearly commented
code explaining what each predicate is doing. If
your code does not work but appears to be close to the
correct solution or your comments are along the correct
lines, then you will receive some credit. If your code is not
clearly commented showing an understanding of what is
happening then you will receive considerably fewer points
than you might have otherwise. */

% WHAT YOU NEED TO HAND IN
/* You should use Canvas to submit a plain text file
named 'sudoku.pl' that contains your sudoku predicate and
any helper predicates. We should be able to run this by
using the tests provided. The file should contain your
name and student numbers, plus a brief summary of
which of the tests you think will work, or any
extra information we will need. This is important,
because if you think that the program should work on all
the tests, but this information is not provided in your
submission (in some place up front where we can easily
see it), then we will assume that it doesn't work. */

/* Keep in mind that you may not use the Constraint
Logic Programming features supplied by SWI-Prolog. */

/* You must submit your solution no later than 6:00pm,
Sunday, April 15, 2017. */

/* ----------- cut here ----------- */

/* include name and student number */

/* This runs all the simple tests. If it
works correctly, you should see three identical
and completed sudoku tables, and finally the
word false (as test0c will fail.) */
test :-
	test0, nl,
	test0a, nl,
	test0b, nl,
	test0c.

/* This is a completly solved solution. */
test0 :-
	L = [
             [9,6,3,1,7,4,2,5,8],
             [1,7,8,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,9,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,5,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) which
should be found very quickly. */
test0a :-
	L = [
             [9,_,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	         [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) and
may take a few seconds to find. */
test0b :-
	L = [
             [9,_,3,1,7,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This one obviously has no solution (column 2 has
two nines in it.) and it may take a few seconds
to deduce this. */
test0c :-
	L = [
             [_,9,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     	 [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* Here is an extra test for you to try. It would be
nice if your program can solve this puzzle, but it's
not a requirement. */

test0d :-
	L = [
             [9,_,3,1,_,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,_,8,1,7,3]],
        sudoku(L),
        printsudoku(L).


/* The next 3 tests are supposed to be progressively
harder to solve. The solver we demonstrated in class
did not find a solution in a reasonable length of time for
any of these, so if you manage to write a solver
that does them in a reasonable length of time,
expect to receive bonus points (what’s a reasonable
length of time?  Let’s call it 5 minutes. (BUT YOU
MUST TELL US IN YOUR SUBMISSION THAT YOUR SOLVER
WORKS ON THESE TESTS OR WE WON'T RUN THESE TESTS
AND YOU WON’T GET THE BONUS POINTS YOU DESERVE.) */
test1 :-
	L = [
             [_,6,_,1,_,4,_,5,_],
             [_,_,8,3,_,5,6,_,_],
             [2,_,_,_,_,_,_,_,1],
             [8,_,_,4,_,7,_,_,6],
	     	 [_,_,6,_,_,_,3,_,_],
             [7,_,_,9,_,1,_,_,4],
             [5,_,_,_,_,_,_,_,2],
             [_,_,7,2,_,6,9,_,_],
             [_,4,_,5,_,8,_,7,_]],
        sudoku(L),
        printsudoku(L).

test2 :-
	L = [
             [_,_,4,_,_,3,_,7,_],
             [_,8,_,_,7,_,_,_,_],
             [_,7,_,_,_,8,2,_,5],
             [4,_,_,_,_,_,3,1,_],
	         [9,_,_,_,_,_,_,_,8],
             [_,1,5,_,_,_,_,_,4],
             [1,_,6,9,_,_,_,3,_],
             [_,_,_,_,2,_,_,6,_],
             [_,2,_,4,_,_,5,_,_]],
        sudoku(L),
        printsudoku(L).

test3 :-
	L = [
             [_,4,3,_,8,_,2,5,_],
	     	 [6,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,1,_,9,4],
             [9,_,_,_,_,4,_,7,_],
             [_,_,_,6,_,8,_,_,_],
             [_,1,_,2,_,_,_,_,3],
             [8,2,_,5,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,5],
             [_,3,4,_,9,_,7,1,_]],
        sudoku(L),
        printsudoku(L).


% print sudoku table
printsudoku([]).
printsudoku([H|T]) :-
	write(H),nl,
	printsudoku(T).


% Expects a list of lists 9 by 9 grid.

% map(X,Y,Z):- validval(X), validval(Y), validval(Z).
%
% solve([H]):- worthy(H).
% solve([H|T]) :- worthy(H), solve(T).
%
% worthy(val(V,map(X,Y,Z))):- validval(V), not(duplicate(val(V,map(X,Y,Z)))).
%
% duplicate(val(V,map(T,Y,_))) :- validval(P), P \= Y, val(V,map(T,P,_)).
% duplicate(val(V,map(T,_,Z))) :- validval(Q), Q \= Z, val(V,map(T,_,Q)).
%
% duplicate(val(V,map(X,T,_))) :- validval(O), O \= X, val(V,map(O,T,_)).
% duplicate(val(V,map(_,T,Z))) :- validval(Q), Q \= Z, val(V,map(_,T,Z)).
%
% duplicate(val(V,map(X,_,T))) :- validval(O), O \= X, val(V,map(O,_,T)).
% duplicate(val(V,map(_,Y,T))) :- validval(P), P \= Y, val(V,map(_,Q,T)).

% assert_val()
% map([H]) :- map_val(H)

% assert block grouping  block(1,map(1,2)).
% assert maps as you move through the the checks.
% everytime you assert map, count the number of times cal
% dynamic(val/2).

sudoku(L) :-
	abolish(val/2),
	dynamic(val/2),
	solve(L,1),
	abolish(val/2).

% Start solving row by row.
solve([[RowElem|RestRow]],RID):-
	solveRow([[RowElem|RestRow]],RID,1),!.
solve([[RowElem|RestRow]|RestROWS],RID):-
	solveRow([RowElem|RestRow],RID,1),
	NRID is RID + 1,
	solve(RestROWS,NRID).

solveRow([H], RID, CID):-
	map(RID,CID,BID),
	instantiate(H,RID,CID,BID),!.
solveRow([H|T], RID, CID):-
	map(RID,CID,BID),
	instantiate(H,RID,CID,BID),
	NCID is CID + 1,
	solveRow(T, RID, NCID).

instantiate(H,RID,CID,BID):-
	val(H,map(RID,CID,BID)). % check if its already mapped
instantiate(H,RID,CID,BID):-
	var(H), % not instantiated, variable is free, find a solution
	validval(H),
	not(duplicate(val(H,map(RID,CID,BID)))),
	myassert(val(H,map(RID, CID,BID))).
instantiate(H,RID,CID,BID):-
	not(duplicate(val(H,map(RID,CID,BID)))),
	assert(val(H,map(RID,CID,BID))).



myassert(X):- assert(X).
myassert(X):- retract(X), fail.

% check if there is a duplicate
duplicate(val(V,map(X,_,_))):- val(V,map(X,_,_)).
duplicate(val(V,map(_,Y,_))):- val(V,map(_,Y,_)).
duplicate(val(V,map(_,_,Z))):- val(V,map(_,_,Z)).

% possible valid values for the puzzle
validval(1).
validval(2).
validval(3).
validval(4).
validval(5).
validval(6).
validval(7).
validval(8).
validval(9).

% mapping block 1
map(1,1,1).
map(1,2,1).
map(1,3,1).
map(2,1,1).
map(2,2,1).
map(2,3,1).
map(3,1,1).
map(3,2,1).
map(3,3,1).

% mapping block 2
map(1,4,2).
map(1,5,2).
map(1,6,2).
map(2,4,2).
map(2,5,2).
map(2,6,2).
map(3,4,2).
map(3,5,2).
map(3,6,2).

% mapping block 3
map(1,7,3).
map(1,8,3).
map(1,9,3).
map(2,7,3).
map(2,8,3).
map(2,9,3).
map(3,7,3).
map(3,8,3).
map(3,9,3).

% mapping block 4
map(4,1,4).
map(4,2,4).
map(4,3,4).
map(5,1,4).
map(5,2,4).
map(5,3,4).
map(6,1,4).
map(6,2,4).
map(6,3,4).

% mapping block 5
map(4,4,5).
map(4,5,5).
map(4,6,5).
map(5,4,5).
map(5,5,5).
map(5,6,5).
map(6,4,5).
map(6,5,5).
map(6,6,5).

% mapping block 6
map(4,7,6).
map(4,8,6).
map(4,9,6).
map(5,7,6).
map(5,8,6).
map(5,9,6).
map(6,7,6).
map(6,8,6).
map(6,9,6).

% mapping block 7
map(7,1,7).
map(7,2,7).
map(7,3,7).
map(8,1,7).
map(8,2,7).
map(8,3,7).
map(9,1,7).
map(9,2,7).
map(9,3,7).

%mapping block 8
map(7,4,8).
map(7,5,8).
map(7,6,8).
map(8,4,8).
map(8,5,8).
map(8,6,8).
map(9,4,8).
map(9,5,8).
map(9,6,8).

%mapping block 9
map(7,7,9).
map(7,8,9).
map(7,9,9).
map(8,7,9).
map(8,8,9).
map(8,9,9).
map(9,7,9).
map(9,8,9).
map(9,9,9).
