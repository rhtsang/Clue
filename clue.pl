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
	initWeapons
.

initRooms :-
	nl,
	writeln('Enter a ROOM that is in play this round (don\'t forget a period after), or type \'done\' to continue'),
	read(Room),
	( Room \= done ->
		assert(room(Room)),
		initRooms
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
	nl,
	writeln('Enter a WEAPON that is in play this round (don\'t forget a period after), or type \'done\' to continue'),
	read(Weapon),
	( Weapon \= done ->
		assert(weapon(Weapon)),
		initWeapons
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

/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
