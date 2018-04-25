/* Gameplay */

/* Initialization */
init :-
	nl,
	writeln('Welcome to your Clue Player Assistant!'),
	writeln('My job is to help you win this game of Clue.'),
	writeln('Let\'s get started!'),

	retractall(room(_)),
	retractall(suspect(_)),
	retractall(weapon(_)),
	retractall(holds(_,_)),

	initRooms,
	initSuspects,
	initWeapons,

	printNotebook
.

initSuspects :-
	assert(suspect(mustard)),
	assert(suspect(scarlet)),
	% assert(suspect(plum)),
	% assert(suspect(green)),
	% assert(suspect(white)),
	assert(suspect(peacock))
.

initWeapons :-
	% initWeaponsHelper(2)
	assert(weapon(knife)),
	assert(weapon(rope)),
	assert(weapon(gun))
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

initRooms :-
	% initRoomsHelper(2)
	assert(room(bath)),
	assert(room(bed)),
	assert(room(lib))
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

play :-
	nl,
	write('Suspect: '),
	read(SuspectCard),
	write('Weapon: '),
	read(WeaponCard),
	write('Room: '),
	read(RoomCard),
	write('You were shown: '),
	read(Card),
	( Card \= done ->
	    ( room(Card) -> retract(room(Card))
	    ; suspect(Card) -> retract(suspect(Card))
	    ; weapon(Card) -> retract(weapon(Card))
	    ; nl, writeln('That doesn\'t seem to be a valid entry. Try again.')
	    )
	; writeln('Let\'s continue!')
	),
	printNotebook
.

solved :-
	findall(S, suspect(S), Slist),
	length(Slist, 1),
	findall(W, weapon(W), Wlist),
	length(Wlist, 1),
	findall(R, room(R), Rlist),
	length(Rlist, 1)
.


start :-
	init,
	removeDealt
.

printNotebook:-
	writeln('Remaining Suspects:'),
	forall(suspect(S), writeln(S)),
	writeln('Remaining Weapons:'),
	forall(weapon(W), writeln(W)),
	writeln('Remaining Rooms:'),
	forall(room(R), writeln(R))
 .

 makeplayer:-
 	forall(suspect(S), assert(holds(pavel, S)))
.

printplayers(X):-
	write(X), write(' holds: '), nl,
	forall(holds(X,S), writeln(S)), nl
.



/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
