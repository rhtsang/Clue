/* Gameplay */

init :-
	nl,
	writeln('Welcome to your Clue Player Assistant!'),
	writeln('My job is to help you win this game of Clue.'),
	writeln('Let\'s get started!'),

	retractall(room(_)),
	retractall(suspect(_)),
	retractall(weapon(_)),

	initRooms,
	initSuspects,
	initWeapons,

	printsetup
.

initRooms :-
	initRoomsHelper(6)
.

initRoomsHelper(X):-
	nl,
	writeln('Enter a ROOM that is in play this round (don\'t forget a period after)'),
	( X > 0 ->
		read(Room),
		assert(room(Room)),
		initRoomsHelper(X-1)
	; nl, writeln('Let\'s continue!')
	)
.

initSuspects :-
	assert(suspect(mustard)),
	assert(suspect(scarlet)),
	assert(suspect(plum)),
	assert(suspect(green)),
	assert(suspect(white)),
	assert(suspect(peacock))
.

initWeapons :-
	initWeaponsHelper(6)
.

initWeaponsHelper(X) :-
	nl,
	writeln('Enter a WEAPON that is in play this round (don\'t forget a period after)'),

	( X > 0 ->
		read(Weapon),
		assert(weapon(Weapon)),
		initWeaponsHelper(X-1)
	; nl, writeln('Let\'s continue!')
	)
.

removeDealt :-
	nl,
	writeln('Enter a card you have been dealt (don\'t forget a period after) so that I know that it wasn\'nt involved in the crime! Or type \'done\' to continue.'),
	read(Card),
	( Card \= done ->
	    ( room(Card) -> retract(room(Card)), removeDealt
	    ; suspect(Card) -> retract(suspect(Card)), removeDealt
	    ; weapon(Card) -> retract(weapon(Card)), removeDealt
	    ; nl, writeln('That doesn\'t seem to be a valid entry. Try again.'), removeDealt
	    )
	; writeln('Let\'s continue!')
	)
.

start :-
	init,
	removeDealt
.

printsetup:-
 writeln('Rooms:'),
 forall(room(R), writeln(R)),
 writeln('Weapons:'),
 forall(weapon(W), writeln(W))
 .

/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
