/* Gameplay */

start :-
	init,
	% makeplayer(answer), /* "answer" player implies possible solution */
	% initPlayers, /* move initPlayers to init?  */
	removeYourHand
.

/* Initialization */
init :-
	nl,
	writeln('Welcome to your Clue Player Assistant!'),
	writeln('My job is to help you win this game of Clue.'),
	writeln('Let\'s get started by setting up the cards!'),
	nl,

	retractall(room(_)),
	retractall(suspect(_)),
	retractall(weapon(_)),
	retractall(player(_)),
	retractall(holds(_,_)),

	initRooms,
	initSuspects,
	initWeapons,
	initPlayers,
	makeplayer(answer)
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

initPlayers:-
	nl,
	writeln('Enter a player name (don\'t forget a period after)! type \'done\' when you added all players.'),
	read(Name),
	( Name \= done -> (makeplayer(Name)),
		initPlayers;
		writeln('Let\'s continue!')
	)
.

/* */
removeDealt :-
	nl,
	writeln('Enter a card you have been dealt (don\'t forget a period after) so that I know that it wasn\'t involved in the crime! Or type \'done\' to continue.'),
	read(Card),
	( Card \= done ->
	    ( room(Card) -> retract(room(Card)), removeDealt
	    ; suspect(Card) -> retract(suspect(Card)), removeDealt
	    ; weapon(Card) -> retract(weapon(Card)), removeDealt
	    ; nl, writeln('That doesn\'t seem to be a valid entry. Try again.'), removeDealt
	    )
	; writeln('Let\'s add players!')
	)
.

% change all HOLDS to MAYHOLD
removeYourHand :-
	nl,
	writeln('Enter a card you have been dealt (don\'t forget a period after) so that I know that it wasn\'t involved in the crime! Or type \'done\' to continue.'),
	read(Card),
	( Card \= done ->
	   forall(holds(player(P), Card), retract(holds(player(P), Card))),
		 removeYourHand
	; writeln('Let\'s continue!!')
	)
.


suggest:-
	nl,
	write('Enter Suspect: '),
	read(SuspectCard),
	write('Enter Weapon: '),
	read(WeaponCard),
	write('Enter Room: '),
	read(RoomCard),
	suggest_help(SuspectCard, WeaponCard, RoomCard)
.

/*
if correct input
then prompt for revealed card
else retry
*/
suggest_help(Suspect, Weapon, Room):-
	(	(suspect(Suspect), weapon(Weapon), room(Room)) ->
		(write('Enter revealed card: '),
		read(Card),
			(	(Card==Suspect; Card==Weapon; Card==Room; Card==none) ->
				(record_guess(Suspect, Weapon, Room, Card))
				;(writeln('invalid revealed card'),
				suggest_help(Suspect, Weapon, Room)))
		)
		;writeln('invalid suggestion. try again'),
		suggest
	)
.

/*record_guess(Suspect, Weapon, Room, Card):-
	writeln('i stopped coding here')
.*/

revealed(Opponent, Card):-
	forall((holds(player(P), Card), P \= Opponent), retract(holds(player(P), Card)))
	% do advanced stuff here to deal with Opponent revealing a card
.

does_not_have(Opponent, Suspect, Weapon, Room):-
	retract(holds(player(Opponent), Suspect)),
	retract(holds(player(Opponent), Weapon)),
	retract(holds(player(Opponent), Room))
.

solved :-
	findall(S, suspect(S), Slist),
	length(Slist, 1),
	findall(W, weapon(W), Wlist),
	length(Wlist, 1),
	findall(R, room(R), Rlist),
	length(Rlist, 1)
.

/* print database on demand  */
notebook:-
	writeln('Remaining Suspects:'),
	forall(suspect(S),
						(write('-'), write(S), write(' | maybe held by: '),
							forall((holds(player(P),S)), (write(P), write(' '))), nl)
				), nl,
	writeln(' Remaining Weapons:'),
	forall(weapon(W),
						(write('-'), write(W), write(' | maybe held by: '),
							forall((holds(player(P),W)), (write(P), write(' '))), nl)
				), nl,
	writeln(' Remaining Rooms:'),
	forall(room(R),
						(write('-'), write(R), write(' | maybe held by: '),
							forall((holds(player(P),R)), (write(P), write(' '))), nl)
				), nl
 .

makeplayer(Name):-
 	assert(player(Name)),
 	forall(suspect(S), 	assert(holds(player(Name), S))),
	forall(weapon(W), 	assert(holds(player(Name), W))),
	forall(room(R), 		assert(holds(player(Name), R))),
	nl
.

printplayers(X):-
	write(X), write(' may hold: '), nl,
	forall((holds(player(X),S), suspect(S)), writeln(S)), nl
.



/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
