/* Gameplay */

start :-
	init,
	readYourHand
	% whoseTurn
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
	retractall(mayhold(_,_)),
	retractall(holds(_,_)),

	initRooms,
	initSuspects,
	initWeapons,
	howManyPlayers,
	makeplayer(envelope) % the goal is to find out what cards are in envelope
.

initSuspects :-
	assert(suspect(mustard)),
	assert(suspect(scarlet)),
	% assert(suspect(plum)),
	% assert(suspect(green)),
	% assert(suspect(white)),
	assert(suspect(peacock))
.

/*
	Reading Weapons
*/
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

/*
	Reading Rooms
*/
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

/*
	Reading Player names
*/
howManyPlayers:-
	nl,
	writeln('How many players are there [2-6]?'),
	read(Number),
	(number(Number), Number<7, Number>1 ->
		initPlayers(Number),
		assert(playercount(Number));
		writeln('Please type a valid number'),
		howManyPlayers)
.

initPlayers(Number):-
	nl,
	(Number < 1 -> writeln('Done reading players.')
		;writeln('Enter player name followed by period'),
		read(Name),
		makeplayer(Name),
		initPlayers(Number-1)
	)
.

/*  not being used rn */
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


/*
	Read the cards in your hand and remove them from notebook
*/
readYourHand :-
	nl,
	writeln('How many cards did you get?'),
	read(Number),
	(number(Number) ->  readYourHandx(Number); readYourHand)
.

readYourHandx(Number) :-
	nl,
	(Number =< 0 -> writeln('Done reading hand.')
		;writeln('Enter a card you have been dealt followed by period'),
		read(Card),
		(not(isCardValid(Card)) ->
			(writeln('There is no such card. Try again'),
			readYourHandx(Number));
			forall(mayhold(P, Card), retract(mayhold(P, Card))),
			readYourHandx(Number-1)
		)
	)
.

isCardValid(Card):-
	suspect(Card);
	weapon(Card);
	room(Card)
.


suggest:-
	writeln('What suggestion did you make?'),
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

% do input validation
suggest_help(Suspect, Weapon, Room):-
	(	(suspect(Suspect), weapon(Weapon), room(Room)) ->
			writeln('Enter revealed card, or \'none\' if none of your suggested cards were shown: '),
			read(Card),
			(	Card==none ->
				% no one show -> must be in envelope -> can accuse here
				writeln('Let\'s continue')
			;	(suspect(Card); weapon(Card); room(Card)) ->
				writeln('Who revealed this card?'),
				read(Opponent),
				player(Opponent),
				%record_guess(Opponent,Card)
				assert(holds(Opponent,Card)),
				retractall(mayhold(_,Card))
			; writeln('invalid revealed card'),
			  suggest_help(Suspect, Weapon, Room)
			)

		;writeln('invalid suggestion. try again'),
		 suggest
	)
.

/*record_guess(Suspect, Weapon, Room, Card):-
	writeln('i stopped coding here')
.*/

revealed(Opponent, Card):-
	forall((mayhold(P, Card), P \= Opponent), retract(mayhold(P, Card)))
	% do advanced stuff here to deal with Opponent revealing a card
.

does_not_have(Opponent, Suspect, Weapon, Room):-
	retract(mayhold(Opponent, Suspect)),
	retract(mayhold(Opponent, Weapon)),
	retract(mayhold(Opponent, Room))
.

solved :-
	findall(S, (suspect(S),mayhold(envelope,S)), Slist),
	length(Slist, 1),
	findall(W, (weapon(W),mayhold(envelope,W)), Wlist),
	length(Wlist, 1),
	findall(R, (room(R),mayhold(envelope,R)), Rlist),
	length(Rlist, 1)
.

/* print database on demand  */
notebook:-
	writeln('Remaining Suspects:'),
	forall(suspect(S),
						(write('-'), write(S), write(' | maybe held by: '),
							forall((mayhold(P,S)), (write(P), write(' '))), nl)
				), nl,
	writeln(' Remaining Weapons:'),
	forall(weapon(W),
						(write('-'), write(W), write(' | maybe held by: '),
							forall((mayhold(P,W)), (write(P), write(' '))), nl)
				), nl,
	writeln(' Remaining Rooms:'),
	forall(room(R),
						(write('-'), write(R), write(' | maybe held by: '),
							forall((mayhold(P,R)), (write(P), write(' '))), nl)
				), nl,
	printOppHands
.

printOppHands :-
	writeln('Opponents\' hands'),
	forall((player(P), P \= envelope),
				 (write(P), write(' holds '), forall(holds(P,C), (write(C), write(' '))), nl)
				)
.

giveSuggestion :-
	suspect(S), mayhold(_,S),
	weapon(W), mayhold(_,W),
	room(R), mayhold(_,R),
	!, write('Maybe '), write(S), write(' did it with a '), write(W), write(' in the '), write(R),
	suggest
.

/*
Creates a player and adds all the possible cards of the player
*/
makeplayer(Name):-
 	assert(player(Name)),
 	forall(suspect(S), 	assert(mayhold(Name, S))),
	forall(weapon(W), 	assert(mayhold(Name, W))),
	forall(room(R), 		assert(mayhold(Name, R))),
	nl
.

printplayers(X):-
	write(X), write(' may hold: '), nl,
	forall((mayhold(X,S), suspect(S)), writeln(S)), nl
.

whoseTurn :-
	writeln('Whose turn is it? Type \'mine\' or the player\'s name'),
	read(P),
	( P == mine ->
		myTurn
	; player(P) ->
		oppTurn
	; writeln('Invalid input, let\'s try again.'),
		whoseTurn
	)
.

myTurn :-
	( solved ->
		writeln('make accusation')
		% TODO
	; writeln('Would you like me to give a possible suggestion, or will you make one of your own? Type \'give\' or \'make\'. Or type \'notes\' to show the notebook'),
		read(Suggest),
		( Suggest == notes ->
			notebook
		;	Suggest == give ->
			giveSuggestion
		; Suggest == make ->
			suggest
		; writeln('Invalid input, let\'s try again'),
			myTurn
		)
	)
	%whoseTurn
.

oppTurn :-
	writeln('What cards did this player suggest?'),
	( write('Enter suspect: '), read(S), suspect(S),
	  write('Enter weapon: '), read(W), weapon(W),
	  write('Enter room: '), read(R), room(R),
	  write('Was a card shown this turn? Enter \'yes\' or \'no\' (and don\'t forget the period after!)'), read(Input),
	  ( Input == yes ->
	    write('Who showed this card?'), read(Opponent),
			retract(mayhold(Opponent,S)),
			asserta(mayhold(Opponent,S)),
			retract(mayhold(Opponent,W)),
			asserta(mayhold(Opponent,W)),
			retract(mayhold(Opponent,R)),
			asserta(mayhold(Opponent,R))
	  ; Input == no ->
			writeln('Oh no, your opponent won. Better luck next time!')
	  ; write('Invalid input, try again.'),
	    oppTurn
	  )
	; writeln('Invalid input, try again.'),
	  oppTurn
	)
.

/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
