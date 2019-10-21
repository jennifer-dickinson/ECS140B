% ECS 140B Assignment 3 Spring 2018
% Jennifer Salas 914329903

-module(funs).
-export([   myremoveduplicates/1,
            myintersection/2,
            mylast/1,
            myreverse/1,
            myreplaceall/3]).

% Code given in class, used in myremoveduplicates/1 and myintersection/2
% Takes in an atom and a list and returns a bool checking if the atom is a
% member of the list. It is tail recursive. The time complexity is O(n).
myelem(_, [])        -> false;
myelem(Item, [Y|Ys]) -> (Item == Y) or (myelem(Item, Ys)).

% -- Problem 1 --
% Takes in a list and removes any duplicates. It is tail recursive and preserves
% order. The time complexity is O(n^2).
myremoveduplicates(List)         -> myremoveduplicates_(List, []).
myremoveduplicates_([], List)    -> myreverse(List);
myremoveduplicates_([H|T], List) ->
    Duplicate = myelem(H, T),
    if
        Duplicate -> myremoveduplicates_(T, List);
        true      -> myremoveduplicates_(T, [H|List])
    end.

% -- Problem 2 --
% Takes in two sets and returns the intersection of the two sets. It is tail
% recursive and preserves order. The time complexit is O(n^2).
myintersection(Set1, Set2)          -> myintersection_(Set1, Set2, []).
myintersection_([],_, Result)       -> myreverse(Result);
myintersection_([H|T], Set, Result) ->
    IsMember = myelem(H, Set) and not myelem(H, Result),
    if
        IsMember -> myintersection_(T, Set, [H|Result]);
        true     -> myintersection_(T, Set, Result)
    end.

% -- Problem 3 --
% Takes in a list and returns the last item. It is tail recursive and the time
% complexity is O(n).
mylast([])    -> [];
mylast([H])   -> [H];
mylast([_|T]) -> mylast(T).

% -- Problem 4 --
% Takes in a list and reverses it. It is tail recursive. The time complexity is
% O(n).
myreverse(List)         -> myreverse_(List, []).
myreverse_([], List)    -> List;
myreverse_([H|T], List) -> myreverse_(T, [H|List]).

% -- Problem 5 --
% Takes in two atoms (replacER and replaceEE) and a list. It replaces every
% occurence of replacee with the replacer and returns the resulting list. It is
% tail recursive and preserves order. The time complexity is O(n).
myreplaceall(ER, EE, List)          -> myreplaceall_(ER, EE, List, []).
myreplaceall_(_,_,[],List)          -> myreverse(List);
myreplaceall_(ER, EE, [EE|T], List) -> myreplaceall_(ER, EE, T, [ER|List]);
myreplaceall_(ER, EE, [H|T], List)  -> myreplaceall_(ER, EE, T, [H|List]).

% % Test cases
% funs:myremoveduplicates("abacad").
% funs:myremoveduplicates([3,2,1,3,2,2,1,1]).
% funs:myintersection("abc","bcd").
% funs:myintersection([3,4,2,1], [5,4,1,6,2]).
% funs:myintersection([],[1,2,3]).
% funs:myintersection("abc","").
% funs:mylast("").
% funs:mylast("b").
% funs:mylast("abcd").
% funs:mylast([1,2,3,4]).
% funs:mylast([]).
% funs:myreverse("").
% funs:myreverse("abc").
% funs:myreverse([1,2,3]).
% funs:myreverse([]).
% funs:myreplaceall(3,7,[7,0,7,1,7,2,7]).
% funs:myreplaceall(3,9,[7,0,7,1,7,2,7]).
