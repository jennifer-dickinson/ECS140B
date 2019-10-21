% Project #1: CLUE
% Author: Gabriella Quattrone (Student ID: 913583423)
% Author: Jenhifer Salas (Student ID: 914329903)

% These are the predicates that will be modified as the game is played.
:- dynamic player/1.
:- dynamic self/1.
:- dynamic has/2.
:- dynamic printhas/2.
:- dynamic maybe/2.
:- dynamic nothas/2.
:- dynamic solution/1.

% players/1 indicates the valid number of players.
players(3).
players(4).
players(5).
players(6).


% suspect/1 indicates the valid names of suspects.
suspect(colonelmustard).
suspect(missscarlett).
suspect(professorplum).
suspect(mrgreen).
suspect(mswhite).
suspect(mrspeacock).


% weapon/1 indicates the valid names of weapons.
weapon(candlestick).
weapon(rope).
weapon(leadpipe).
weapon(wrench).
weapon(knife).
weapon(pistol).


% room/1 indicates the valid names of rooms.
room(kitchen). % Room 1
room(ballroom). % Room 2
room(billiardroom). % Room 3
room(study). % Room 4
room(conservatory). % Room 5
room(diningroom). % Room 6
room(library). % Room 7
room(lounge). % Room 8
room(hall). % Room 9


% recordplayers is one of the starting functions. It validates the number of
% players in the game and subsequently calls recordhelper to keep track of
% the playable characters.
recordplayers:-
    write('
        How many players are in the game? There must be 3-6 players.'),nl,
    read(NUMPLAYERS),
    recordplayer(NUMPLAYERS).

recordplayer(NUM) :-
    players(NUM),
    assert(playercount(NUM)),
    write("
        We'll ask you to name each player in the order that they'll take
        turns.\n
        You can enter any of these:
            colonelmustard
            missscarlett
            mrgreen
            mrspeacock
            mswhite
            professorplum"),nl,
    recordhelper(NUM),!.

% This predicate occurs if the user has entered an invalid number of players.
recordplayer(_) :-
    write("That number does not seem right."), nl, recordplayers.


% recordhelper is invoked using record players. It requests the name of a
% player from the user, validates it, and attempts to record it. It asks for
% additional players until the parameter NUMPLAYERS is decremented to 0.
recordhelper(0):-!.
recordhelper(NUMPLAYERS) :-
    write('\nEnter Player: '),nl,
    read(PLAYER),
    setplayer(PLAYER),
    ONELESS is NUMPLAYERS - 1,
    recordhelper(ONELESS).
recordhelper(NUMPLAYERS) :- recordhelper(NUMPLAYERS). % In case there was incorrect input


% setplayer records a given player into a database and handles three conditions:
%   1. The player is valid and not yet added to the database
%   2. The player is valid but added to the database
%   3. The player is invalid
setplayer(PLAYER) :-
    not(player(PLAYER)),
    suspect(PLAYER),
    safeassert(player(PLAYER)),
    write(PLAYER),
    write(' was added to the game.'),nl, !.
setplayer(PLAYER) :-
    suspect(PLAYER),
    write(PLAYER),
    write(' was already added to the game. Try again.'),nl,
    !,fail.
setplayer(PLAYER) :- write(PLAYER),
    write(' is not a valid player!'),nl,fail.


% setself is called after recording all the players in the game and allows
% the user to identify as one of the players.
setself:- write('
        Of the players you entered, which one is you?\n'),nl,
    read(PLAYER), setself(PLAYER).
setself(PLAYER) :- player(PLAYER), safeassert(self(PLAYER)),!.
setself(PLAYER) :-
    write(PLAYER),
    write(' was not previously recorded.\nTry again.\n'),nl,
    read(NEWPLAYER),
    setself(NEWPLAYER).


% safeassert/1 checks that a predicate has not been declared previously before asserting
% the predicate into the database.
% not/1 negates a boolean value, so not(true) = false and not(false) = true, therefore assert.
safeassert(X) :- not(X), assert(X). %
safeassert(X) :- X.


% sethand asks the player:
% how many cards they have,
% what cards, and
% sets what they do not have based on that.
sethand:- write('
        How many cards do you have?\n'),nl,
    read(NUMCARDS), number(NUMCARDS), NUMCARDS \= 0,
    write('
        Enter each card in the prompt below. You can enter any of the following:
            colonelmustard    candlestick   kitchen         library
            missscarlett      rope          ballroom        lounge
            mrgreen           leadpipe      billiardroom     hall
            mrspeacock        wrench        study
            mswhite           knife         conservatory
            professorplum     pistol        diningroom
    '),
    sethand(NUMCARDS), !.
sethand:- write('Invalid number of cards. Enter a value greater than 0.'),nl, sethand.
sethand(0):- setdonothaves,!. % Base Case
sethand(NUMCARDS):-
    record,
    ONELESS is NUMCARDS - 1,
    sethand(ONELESS),!.
sethand(NUMCARDS):- sethand(NUMCARDS). % In case there was incorrect input


% setdonothaves sets the cards the player definitely does not have based on former input.
setdonothaves:-
    forall(weapon(WEAPON),setdonothaves(WEAPON)),
    forall(room(ROOM),setdonothaves(ROOM)),
    forall(suspect(SUSPECT),setdonothaves(SUSPECT)).
setdonothaves(ITEM):-
    self(SELF),
    not(has(SELF,ITEM)),
    safeassert(nothas(SELF,ITEM)).
setdonothaves(_).


% validcard checks if a card is a valid card.
validcard(CARD) :- suspect(CARD); weapon(CARD); room(CARD).
validcard(CARD) :- write(CARD), write(' is not a valid card.'),nl,!, fail.


% suggest/3 is the users interface for making suggestion in game
suggest(SUSPECT, WEAPON, ROOM):-
    suspect(SUSPECT),
    weapon(WEAPON),
    room(ROOM),
    write('Did anyone respond with a card to the suggestion? (Answer "yes." or "no.".)\n'),
    read(ANSWER),nl,
    ask(me, ANSWER, SUSPECT, WEAPON, ROOM),
    checkforsolution,
    write('Cluesheet has been updated.\n').


% record/0, record/1, and record/2 are used in the background of the app
% and are never directly used by the user. They are used to record
% card information and initialize the user prompt.
record:-
    write('\nEnter name of card: '),nl,
    read(CARD),
    record(CARD).
record(CARD) :-
    validcard(CARD),!,
    self(PLAYER),
    asserthas(PLAYER,CARD),!.
record(_) :- record.
record(PLAYER, CARD):-
    validcard(PLAYER),
    validcard(CARD),!,
    asserthas(PLAYER, CARD),!.
record(_,_) :-
    write('One of the values you entered is invalid. Please try again.'),nl,!.
% When a player makes a suggestion we need to do the following:
%   1. record who asked the question and what they asked for
%   2. record any responders
%   3. Place question marks under the responders
%       If another player has a card that was suggested, it is in the record
%       cards, so don't place a question mark.
%       If two cards are known to belong to someone else, record that the
%       responder has the third card.
record(PLAYER, SUSPECT, WEAPON, ROOM):-
    player(PLAYER),
    suspect(SUSPECT),
    weapon(WEAPON),
    room(ROOM),!,
    write('Did anyone respond with a card to the suggestion? (Answer "yes." or "no.".)\n'),
    read(ANSWER),nl,
    ask(PLAYER,ANSWER, SUSPECT, WEAPON, ROOM),
    checkforsolution,
    write('Cluesheet has been updated.\n').
record(_,_,_,_):-
   write('One of the values you entered is invalid. Please try again.'),nl,!.


% Records or checks a known solution.
answer(suspect):-
    suspect(SUSPECT), solution(SUSPECT),
    write(SUSPECT), write(' is the suspect!\n'), !.
answer(weapon):- weapon(WEAPON), solution(WEAPON),
    write(WEAPON), write(' is the weapon!\n'), !.
answer(room):- room(ROOM), solution(ROOM),
    write(ROOM), write(' is the room!\n'), !.

answer(suspect):-
    aggregate_all(count, and(suspect(SUSPECT), has(_,SUSPECT)), NUM),
    NUM = 5,
    weapon(SOLUTION),
    not(has(_,SOLUTION)),
    write(SOLUTION), write(' was inferred as the murderer!\n'),
    forall(player(PLAYER),safeassert(nothas(PLAYER,SOLUTION))),
    safeassert(solution(SOLUTION)),!.
answer(weapon):-
    aggregate_all(count, and(weapon(WEAPON), has(_,WEAPON)), NUM),
    NUM = 5,
    weapon(SOLUTION),
    not(has(_,SOLUTION)),
    write(SOLUTION), write(' was inferred as the weapon used to commit murder!\n'),
    forall(player(PLAYER),safeassert(nothas(PLAYER,SOLUTION))),
    safeassert(solution(SOLUTION)),!.
answer(room):-
    aggregate_all(count, and(room(ROOM), has(_,ROOM)), NUM),
    NUM = 8,
    room(SOLUTION),
    not(has(_,SOLUTION)),
    write(SOLUTION), write(' was inferred as the crime scene!\n'),
    forall(player(PLAYER),safeassert(nothas(PLAYER,SOLUTION))),
    safeassert(solution(SOLUTION)),!.

answer(suspect):-
    suspect(ITEM),
    forall(player(PLAYER), nothas(PLAYER,ITEM)),
    not(var(ITEM)), % Returns true if a variable is free
    write(ITEM),
    write(' must be the suspect!\n'),
    safeassert(solution(ITEM)),!.
answer(suspect):-
    write('Not enough information to determine the suspect.\n').
answer(weapon):-
    weapon(ITEM),
    forall(player(PLAYER), nothas(PLAYER,ITEM)),
    not(var(ITEM)), % Returns true if a variable is free
    write(ITEM),
    write(' must be the weapon!\n'),
    safeassert(solution(ITEM)),!.
answer(weapon):-
    write('Not enough information to determine the weapon used.\n').
answer(room):-
    room(ITEM),
    forall(player(PLAYER), nothas(PLAYER,ITEM)),
    not(var(ITEM)), % Returns true if a variable is free
    write(ITEM),
    write(' must be the room!\n'),
    safeassert(solution(ITEM)),!.
answer(room):-
    write('Not enough information to determine the scene of crime.\n').

% Check that there is a solution found, if yes, tell player.
checkforsolution:-
    checkforsolution(suspect, SUSPECT),
    checkforsolution(weapon, WEAPON),
    checkforsolution(room, ROOM),
    write('Go for the accusation!'),nl,
    write('The suspect is '), write(SUSPECT),nl,
    write('The weapon is '), write(WEAPON),nl,
    write('The room is '), write(ROOM),nl,!.

checkforsolution:-
    answer(suspect),
    answer(weapon),
    answer(room).


checkforsolution(suspect, SUSPECT):-
    suspect(SUSPECT), solution(SUSPECT).
checkforsolution(weapon,WEAPON):-
    weapon(WEAPON), solution(WEAPON).
checkforsolution(room,ROOM):-
    room(ROOM), solution(ROOM).


ask(PLAYER, no, SUSPECT, WEAPON, ROOM):-
    turneval(PLAYER,TURNORDER),
    recordnot([], SUSPECT, WEAPON, ROOM, TURNORDER).

ask(me, no, SUSPECT, WEAPON, ROOM):-
	self(ME),
	turneval(ME, TURNORDER),
	recordnot([], SUSPECT, WEAPON, ROOM, TURNORDER).

ask(me, yes, SUSPECT, WEAPON, ROOM) :-
    write('Who responded?\n'),
    read(RESPONDER),nl,
    player(RESPONDER),!,
    write('What did they respond with?\n'),
    read(CARD),
    askedfor(CARD, SUSPECT, WEAPON, ROOM),
    record(RESPONDER, CARD), % The responder either has it, they don't, or they might
    self(ME),
    turneval(ME, TURNORDER), % Check the people after the asker and stop when we reach the responder
    recordnot(RESPONDER, SUSPECT, WEAPON, ROOM, TURNORDER),!. % Asserts those players who don't have these cards

ask(me, yes, SUSPECT, WEAPON, ROOM):-
    write("That is not a card you asked for!\n"),
    ask(me, yes, SUSPECT, WEAPON, ROOM),!.

ask(PLAYER, yes, SUSPECT, WEAPON, ROOM) :-
    write('Who responded?\n'),
    read(RESPONDER),nl,
    player(RESPONDER),!,
    responded(RESPONDER, SUSPECT, WEAPON, ROOM), % The responder either has it, they don't, or they might
    turneval(PLAYER, TURNORDER), % Check the people after the asker and stop when we reach the responder
    recordnot(RESPONDER, SUSPECT, WEAPON, ROOM, TURNORDER). % Asserts those players who don't have these cards


askedfor(CARD,SUSPECT,_,_):-
    CARD = SUSPECT.
askedfor(CARD,_,WEAPON,_):-
    CARD = WEAPON.
askedfor(CARD,_,_,ROOM):-
    CARD = ROOM.


recordnot(_,_,_,_,[]).
recordnot(STOP, SUSPECT, _, _, [STOP|_]) :-
    aggregate_all(count, nothas(_,SUSPECT), NUM),
    playercount(NUM),
    safeassert(solution(SUSPECT)),
    write(SUSPECT), write(' is the suspect!').
recordnot(STOP, _, WEAPON, _, [STOP|_]) :-
    aggregate_all(count, nothas(_,WEAPON), NUM),
    playercount(NUM),
    safeassert(solution(WEAPON)),
    write(WEAPON), write(' is the weapon!').
recordnot(STOP, _, _, ROOM, [STOP|_]) :-
    aggregate_all(count, nothas(_,ROOM), NUM),
    playercount(NUM),
    safeassert(solution(ROOM)),
    write(ROOM), write(' is the room!').
recordnot(STOP,_,_,_,[STOP|_]).
recordnot(STOP, SUSPECT, WEAPON, ROOM, [OTHER|REST]) :-
    safeassert(nothas(OTHER, SUSPECT)), saferetract(maybe(OTHER, SUSPECT)),
    safeassert(nothas(OTHER, WEAPON)), saferetract(maybe(OTHER, WEAPON)),
    safeassert(nothas(OTHER, ROOM)), saferetract(maybe(OTHER, ROOM)),
    recordnot(STOP, SUSPECT, WEAPON, ROOM, REST).


saferetract(X):- X, retract(X). % Check if X is true.
saferetract(X):- not(X). % Otherwise, no need to retract.


% When we know all the suggested cards have already been recorded.
responded(_, SUSPECT, WEAPON, ROOM) :-
    has(_,SUSPECT),
    has(_,WEAPON),
    has(_,ROOM),!.

% % When suspect has not been recorded, but the other two have.
% responded(RESPONDER, SUSPECT, WEAPON, ROOM):-
%     has(RESPONDER, ROOM),
%     nothas(RSPONDER, WEAPON),


% The responder, does not have two of the ITEMs and someone else has the weapon
% or the responder definitely does not have the two ITEMs
% then they must have the third item
responded(RESPONDER, SUSPECT, WEAPON, ROOM) :-
    or(and(not(has(RESPONDER,WEAPON)), has(_,WEAPON)), nothas(RESPONDER, WEAPON)),
    or(and(not(has(RESPONDER,ROOM)), has(_,ROOM)), nothas(RESPONDER,ROOM)),
    not(has(_,SUSPECT)),
    not(nothas(RESPONDER, SUSPECT)),
    asserthas(RESPONDER, SUSPECT),
    write('Cluefinder has made an inference that '),
    write(RESPONDER),
    write(' holds the card '),
    write(SUSPECT),
    write('.'),nl,!.

% When weapon has not been recorded, but the other two have.
responded(RESPONDER, SUSPECT, WEAPON, ROOM) :-
    or(and(not(has(RESPONDER,SUSPECT)), has(_,SUSPECT)), nothas(RESPONDER, SUSPECT)),
    or(and(not(has(RESPONDER,ROOM)), has(_,ROOM)), nothas(RESPONDER,ROOM)),
    not(has(_,WEAPON)),
    not(nothas(RESPONDER,WEAPON)),
    asserthas(RESPONDER, WEAPON),
    write('Cluefinder has made an inference that '),
    write(RESPONDER),
    write(' holds the card '),
    write(WEAPON),
    write('.'),nl,!.

% When room has not been recorded, but the other two have.
responded(RESPONDER, SUSPECT, WEAPON, ROOM) :-
    or(and(not(has(RESPONDER,SUSPECT)), has(_,SUSPECT)), nothas(RESPONDER, SUSPECT)),
    or(and(not(has(RESPONDER,WEAPON)), has(_,WEAPON)), nothas(RESPONDER, WEAPON)),
    not(has(_, ROOM)),
    not(nothas(RESPONDER, ROOM)),
    asserthas(RESPONDER, ROOM),
    write('Cluefinder has made an inference that '),
    write(RESPONDER),
    write(' holds the card '),
    write(ROOM),
    write('.'), nl,!.

% All other cases fail, so we assert that this responder might have any one of these cards
responded(RESPONDER, SUSPECT, WEAPON, ROOM) :-
    maybeorhas(RESPONDER, SUSPECT),
    maybeorhas(RESPONDER, WEAPON),
    maybeorhas(RESPONDER, ROOM).

or(X,_):- X.
or(_,Y):- Y.
and(X,Y):- X,Y.

asserthas(RESPONDER, CARD):-
    not(has(_, CARD)), % Make sure that no-one already has the card.
    saferetract(maybe(_,CARD)),
    forall(player(X),safeassert(nothas(X,CARD))),
    saferetract(nothas(RESPONDER,CARD)),
    safeassert(has(RESPONDER, CARD)).
asserthas(RESPONDER, CARD):-
    write(CARD), write(' already recorded for '), write(RESPONDER), nl.


% Checks if any one else already has the card before recording it as a maybe
maybeorhas(_, CARD):- has(_,CARD), !.
maybeorhas(RESPONDER, CARD):- safeassert(maybe(RESPONDER, CARD)).


% cluesheet works by checking if a player has a card. It checks this thorugh the
% dynamic predicate has/2 which called by a printer predicate(player, card).
% has/2 takes in two paremeters player and card and is true if a player has the card
cluesheet:-
write("             "), printplayer(colonelmustard), printplayer(mrspeacock), printplayer(professorplum),printplayer(mswhite), printplayer(missscarlett), printplayer(mrgreen), printseperator,
write("Col. Mustard "), printhas(colonelmustard,colonelmustard), printhas(mrspeacock,colonelmustard), printhas(professorplum, colonelmustard), printhas(mswhite, colonelmustard), printhas(missscarlett, colonelmustard), printhas(mrgreen, colonelmustard), printseperator,
write("Ms. Scarlett "), printhas(colonelmustard,missscarlett), printhas(mrspeacock,missscarlett), printhas(professorplum, missscarlett), printhas(mswhite, missscarlett), printhas(missscarlett, missscarlett), printhas(mrgreen, missscarlett), printseperator,
write("Mr. Green    "), printhas(colonelmustard,mrgreen), printhas(mrspeacock,mrgreen), printhas(professorplum, mrgreen), printhas(mswhite, mrgreen), printhas(missscarlett, mrgreen), printhas(mrgreen, mrgreen), printseperator,
write("Mrs. Peacock "), printhas(colonelmustard,mrspeacock), printhas(mrspeacock,mrspeacock), printhas(professorplum, mrspeacock), printhas(mswhite, mrspeacock), printhas(missscarlett, mrspeacock), printhas(mrgreen, mrspeacock), printseperator,
write("Ms. White    "), printhas(colonelmustard,mswhite), printhas(mrspeacock,mswhite), printhas(professorplum, mswhite), printhas(mswhite, mswhite), printhas(missscarlett, mswhite), printhas(mrgreen, mswhite), printseperator,
write("Prof. Plum   "), printhas(colonelmustard,professorplum), printhas(mrspeacock,professorplum), printhas(professorplum, professorplum), printhas(mswhite, professorplum), printhas(missscarlett, professorplum), printhas(mrgreen, professorplum), printseperator,
write("Candlestick  "), printhas(colonelmustard,candlestick), printhas(mrspeacock,candlestick), printhas(professorplum, candlestick), printhas(mswhite, candlestick), printhas(missscarlett, candlestick), printhas(mrgreen, candlestick), printseperator,
write("Rope         "), printhas(colonelmustard,rope), printhas(mrspeacock,rope), printhas(professorplum, rope), printhas(mswhite, rope), printhas(missscarlett, rope), printhas(mrgreen, rope), printseperator,
write("Lead Pipe    "), printhas(colonelmustard,leadpipe), printhas(mrspeacock,leadpipe), printhas(professorplum, leadpipe), printhas(mswhite, leadpipe), printhas(missscarlett, leadpipe), printhas(mrgreen, leadpipe), printseperator,
write("Wrench       "), printhas(colonelmustard,wrench), printhas(mrspeacock,wrench), printhas(professorplum, wrench), printhas(mswhite, wrench), printhas(missscarlett, wrench), printhas(mrgreen, wrench), printseperator,
write("Knife        "), printhas(colonelmustard,knife), printhas(mrspeacock, knife), printhas(professorplum, knife), printhas(mswhite, knife), printhas(missscarlett, knife), printhas(mrgreen, knife), printseperator,
write("Pistol       "), printhas(colonelmustard,pistol), printhas(mrspeacock,pistol), printhas(professorplum, pistol), printhas(mswhite, pistol), printhas(missscarlett, pistol), printhas(mrgreen, pistol), printseperator,
write("Kitchen      "), printhas(colonelmustard,kitchen), printhas(mrspeacock,kitchen), printhas(professorplum, kitchen), printhas(mswhite, kitchen), printhas(missscarlett, kitchen), printhas(mrgreen, kitchen), printseperator,
write("Ballroom     "), printhas(colonelmustard,ballroom), printhas(mrspeacock,ballroom), printhas(professorplum, ballroom), printhas(mswhite, ballroom), printhas(missscarlett, ballroom), printhas(mrgreen, ballroom), printseperator,
write("Billiard Room"), printhas(colonelmustard,billiardroom), printhas(mrspeacock,billiardroom), printhas(professorplum, billiardroom), printhas(mswhite, billiardroom), printhas(missscarlett, billiardroom), printhas(mrgreen, billiardroom), printseperator,
write("Study        "), printhas(colonelmustard,study), printhas(mrspeacock,study), printhas(professorplum, study), printhas(mswhite, study), printhas(missscarlett, study), printhas(mrgreen, study), printseperator,
write("Conservatory "), printhas(colonelmustard,conservatory), printhas(mrspeacock,conservatory), printhas(professorplum,conservatory), printhas(mswhite, conservatory), printhas(missscarlett, conservatory), printhas(mrgreen, conservatory), printseperator,
write("Dining Room  "), printhas(colonelmustard,diningroom), printhas(mrspeacock,diningroom), printhas(professorplum, diningroom), printhas(mswhite, diningroom), printhas(missscarlett, diningroom), printhas(mrgreen, diningroom), printseperator,
write("Library      "), printhas(colonelmustard,library), printhas(mrspeacock,library), printhas(professorplum, library), printhas(mswhite, library), printhas(missscarlett, library), printhas(mrgreen, library), printseperator,
write("Lounge       "), printhas(colonelmustard,lounge), printhas(mrspeacock,lounge), printhas(professorplum, lounge), printhas(mswhite, lounge), printhas(missscarlett, lounge), printhas(mrgreen, lounge), printseperator,
write("Hall         "), printhas(colonelmustard,hall), printhas(mrspeacock,hall), printhas(professorplum, hall), printhas(mswhite, hall), printhas(missscarlett, hall), printhas(mrgreen, hall), printseperator,!.


% printhas/2 prints dialog based on whether a specified player has a specified card
%   if the player has a card, print X
%   if the player does not have the card, but any other player has it, print a blank
%   if no player has the card, print a strike
printhas(PLAYER,_) :- not(player(PLAYER)),!.
printhas(PLAYER,CARD) :- has(PLAYER,CARD), write('|     O     '),!.
printhas(PLAYER,CARD) :- nothas(PLAYER,CARD), write('|     X     '),!.
printhas(PLAYER,CARD) :- maybe(PLAYER,CARD), write('|     ?     '),!.
printhas(_,_) :- write('|           '),!.
% printplayer/1 prints the name of the player on the cluesheet if they where
%   declared as a player
printplayer(PLAYER) :- player(PLAYER), printheader(PLAYER,X), write(X).
printplayer(_).

% printheader/2 matches a player with their corresponding header
printheader(colonelmustard,"|ColMustard ").
printheader(mrspeacock,"|MrsPeacock ").
printheader(professorplum, "| ProfPlum  ").
printheader(mswhite,"|  MsWhite  ").
printheader(missscarlett,"|MsScarlett ").
printheader(mrgreen,"|  MrGreen  ").


% printseperator/0 prints a row of seperators, one seperator for each player
printseperator:-
    aggregate_all(count, player(_), Count), % Built-in predicate that counts all true values for the given predicate.
    write('\n-------------'),
    printseperator(Count),write('\n').
printseperator(0).
printseperator(N):-
    write('|-----------'),
    Next is N - 1,
    printseperator(Next).


turneval(PLAYER, TURNORDER):-
    player(PLAYER),
    findall(X,player(X),L),
    putfront(PLAYER, L, TURNORDER).


% Generates a list of players after the given player in turn order per round.
putfront(PLAYER, PLAYERLIST, TURNORDER):-
    putfront(PLAYER, [], PLAYERLIST, TURNORDER), !.
% This is the base case if the player is already at the head of the list.
putfront(P, BP, [P|AP], TURNORDER):- % P = player
    append(AP,BP, TURNORDER).
putfront(P, BP, [NP|AP], TURNORDER):-
    append(BP, [NP], BNP), % BP = before player, NP = not player, BNP = before not player
    putfront(P, BNP, AP, TURNORDER).


help:-
    help(suspect,SUGGEST_SUSPECT),  %if this fails, suggest someone we already know, but as a last resort
    help(weapon, SUGGEST_WEAPON),
    help(room, SUGGEST_ROOM),
    write('You should suggest the following in your game and enter the following command:'),nl,
    write('suggest('),
    write(SUGGEST_SUSPECT),
    write(', '),
    write(SUGGEST_WEAPON),
    write(', '),
    write(SUGGEST_ROOM),
    write(').\n'),!.
    % write('If that room does not work, try any of these:\n'),

% alternaterooms.
% help/2 either suggests a card in our hand since we already know the suspect/weapon/room,
% so we play dumb, OR
% suggest a card that we aren't sure anybody has yet, in the case that we don't yet know the solution
help(suspect,SUGGEST):-
    suspect(SUSPECT), solution(SUSPECT), self(US), suspect(SUGGEST), SUGGEST \= SUSPECT, has(US, SUGGEST),
    write('The suspect is '), write(SUSPECT), write(' but we are going to play dumb.\n').
help(suspect,SUSPECT):-
    suspect(SUSPECT), not(has(_,SUSPECT)).
help(weapon,SUGGEST):-
    weapon(WEAPON), solution(WEAPON), self(US), weapon(SUGGEST), SUGGEST \= WEAPON, has(US, SUGGEST),
    write('The weapon is '), write(WEAPON), write(' but we are going to play dumb.\n').
help(weapon,WEAPON):-
    weapon(WEAPON), not(has(_,WEAPON)).
help(room,SUGGEST):-
    room(ROOM), solution(ROOM), self(US), room(SUGGEST), SUGGEST \= ROOM, has(US, SUGGEST),
    write('The room is '), write(ROOM), write(' but we are going to play dumb.\n').
help(room,ROOM):-
    room(ROOM), not(has(_,ROOM)).

    % to make a suggestion for a given type
    %   1. check that we do not know a solution
    %       if we know, then suggest someone in our hand
    %   2. otherwise, find out if there is a card no one has

:- write("
        Welcome to Clue Helper! We'll turn your clues into answers!\n
        First, we'll need some information about your game.\n
        Please answer the following questions and write your answers
        using only lowercase letters or numbers followed by a period.\n"),
    recordplayers,
    setself,
    sethand,
    write('
        You are all set!\n
        To see your cluesheet, enter the command "cluesheet."
        When an opponent asks a question write
            "record(PLAYER, SUSPECT, WEAPON, ROOM)." - where
            PLAYER is the person who asked the question,
            SUSPECT is the person they are asking for,
            WEAPON is the weapon they are asking for, and
            ROOM is the room they are inside of.
            We will try to guess an answer for you!.
        On your turn, to make a suggestion, write
            "suggest(SUSPECT, WEAPON, ROOM)."
        If you ever need help making a new suggestion, type
            "help."
        For more information about variables such as "knife" or "colonelmustard", please look at the README.'),nl,nl.
