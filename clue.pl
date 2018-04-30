/*
ECS 140B
Clue Project
Spring 2018
Raymond Tsang 912868864
Pavel Kuzkin 912807074
*/

/*
Welcome to our Clue Player Assistant!

This project is designed to aid the user in winning his/her game of Clue by 
keeping track of information gathered throughout the course of the game and
helping the user make decisions based on this information.

To get started, load the clue.pl file into the Prolog knowledge base. Then type
'start.' and follow the on-screen prompts to initialize the the assistant with
the game's information. After this is done, the user can type 'myTurn.' or
'oppTurn.' to record information gathered and inferred on the user's turn or
an opponent's turn. The user can also type 'notebook.' to see the current
state of the game, i.e. which cards have been eliminated from contention.
(The player 'envelope' indicates that the card could be in the envelope,
i.e. part of the crime.) 

notebook.
	This predicate allows the user to see the contents of the database on-demand.
	For each card in the game, it prints a list of all possible players who could
	be holding it, including the player him/herself. The player 'envelope'
	indicates that the card could be in the envelope, i.e. part of the crime.

myTurn.
	This predicate is used to record information the user gains during his/her
	turn. It begins by checking if the case is solved (i.e. there is only one
	possible combination of suspects, weapons, and rooms left). If the case is
	solved, it tells the user what accusation to make. If not, it can give the
	user a suggestion to make before prompting the user for what suggestion they
	made, and if a card was revealed to him/her. After the user enters this
	information, the assistant updates the database based on the knowledge
	gathered and inferred.

oppTurn.
	Similar to myTurn, this predicate is used to record information that is
	inferred from an opponent's turn, and then updates the database in order
	to provide better suggestions to give the user.

*/


/* Gameplay */

% called by the user to initiliaze the assistant
start :-
	init,
	readYourHand
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
	retractall(playercount(_)),

	initRooms(9),
	initSuspects,
	initWeapons(6),
	howManyPlayers,
	makeplayer(envelope) % the goal is to find out what cards are in envelope
.

initSuspects :-
	assert(suspect(mustard)),
	assert(suspect(scarlet)),
	assert(suspect(plum)),
	assert(suspect(green)),
	assert(suspect(white)),
	assert(suspect(peacock))
.

/*
	Reading weapons from user input
*/
initWeapons(X) :-
	nl,
	writeln('Enter a WEAPON that is in play this round (don\'t forget a period after)'),

	( X > 0 ->
		read(Weapon),
		assert(weapon(Weapon)),
		initWeapons(X-1)
	; nl, writeln('Let\'s continue!')
	)
.

/*
	Reading rooms from user input
*/
initRooms(X):-
	nl,
	writeln('Enter a ROOM that is in play this round (don\'t forget a period after)'),
	( X > 0 ->
		read(Room),
		assert(room(Room)),
		initRooms(X-1)
	; nl, writeln('Let\'s continue!')
	)
.

/*
	Reading player names from user input
*/
howManyPlayers:-
	nl,
	writeln('How many OTHER players are there [1-5]?'),
	read(Number),
	(number(Number), Number<6, Number>0 ->
		initPlayers(Number),
		assert(playercount(Number));
		writeln('Please type a valid number'),
		howManyPlayers)
.

initPlayers(0):- writeln('Done reading players.').
initPlayers(Number):-
	nl,
	Number > 0,
	writeln('Enter player name followed by period'),
	read(Name),
	makeplayer(Name),
	X is Number-1,
	initPlayers(X)
.

/*
	Read the cards in your hand and remove them from the assistant's database,
	because they can't be in the envelope (i.e. part of the crime)
*/
readYourHand :-
	nl,
	writeln('How many cards did you get?'),
	read(Number),
	(number(Number) ->  readYourHandx(Number); readYourHand)
.

readYourHandx(0) :- writeln('Done reading hand.').
readYourHandx(Number) :-
	nl,
	Number > 0,
	writeln('Enter a card you have been dealt followed by period'),
	read(Card),
	(isCardValid(Card) ->
		(forall(mayhold(P, Card), retract(mayhold(P, Card))),
		X is Number-1,
		readYourHandx(X));
		(writeln('There is no such card. Try again'),
		readYourHandx(Number))
	)
.

/*
	checks if a card is valid
*/
isCardValid(Card):-
	suspect(Card);
	weapon(Card);
	room(Card)
.

/*
	each suggest func checks if input is valid and starts the next func.
	Once suspect, weapon and room inputs are recorded suggestResponse records
	response.
*/
suggest:-
	nl,
	writeln('What suggestion are you making?'),
	suggestSuspect
.

suggestSuspect:-
	nl,
	write('Suspects: '),
	forall(suspect(S), (write(S), write('. '))),
	nl,
	write('Enter Suspect: '),
	read(Suspect),
	(suspect(Suspect) -> suggestWeapon(Suspect);
		(writeln('No such suspect. Try again'),
		suggestSuspect))
.

suggestWeapon(Suspect):-
	nl,
	write('Weapons: '),
	forall(weapon(W), (write(W), write('. '))),
	nl,
	write('Enter Weapon: '),
	read(Weapon),
	(weapon(Weapon) -> suggestRoom(Suspect, Weapon);
		(writeln('No such weapon. Try again'),
		suggestWeapon(Suspect)))
.

suggestRoom(Suspect, Weapon):-
	nl,
	write('Rooms: '),
	forall(room(R), (write(R), write('. '))),
	nl,
	write('Enter Room: '),
	read(Room),
	(room(Room) -> (playercount(X), suggestResponsePlayer(Suspect, Weapon, Room, X));
		(writeln('No such room. Try again'),
		suggestRoom(Suspect, Weapon)))
.

suggestResponsePlayer(_, _, _, 0) :- writeln('Collected all responses').
suggestResponsePlayer(Suspect, Weapon, Room, CountPlayers):-
	nl,
	CountPlayers > 0,
	write('Players:'),
	forall((player(P), P \= envelope), (write(P), write('. '))),
	nl,
	write('Responding player: '),
	read(Name),
	(player(Name) -> suggestResponseCard(Name,Suspect, Weapon, Room);
		(writeln('no such player. Try again'),
		suggestResponsePlayer(Suspect, Weapon, Room, CountPlayers)))
.

suggestResponseCard(Name,Suspect, Weapon, Room):-
	nl,
	write('Options: [nothing. '),
	write(Suspect), write('. '),
	write(Weapon), write('. '),
	write(Room), writeln('] '),
	write('What did '), write(Name), write(' show you?'),
	read(Ans),
	( % case: if nothing to reveal
		Ans == nothing ->
		(nl,write('That means that '),
		write(Name),
		write(' does not have any of those cards! Recording in notebook'),
		does_not_have(Name,Suspect, Weapon, Room)
		);

			( % case: else if revealed a card
			(Ans == Suspect; Ans == Weapon; Ans == Room)->
				(write('That means that  '),
				forall((player(P), P \= Name), (write(P), write(' '))),
				write('are not holding '),
				writeln(Ans),
				write('Recording that '),
				write(Name),
				write(' is holding '),
				writeln(Ans),
				revealed(Name, Ans)
				);

				% case: else invalid input
				(writeln('invalid card. try again'),
				suggestResponseCard(Name,Suspect, Weapon, Room))
			)
	)
.

revealed(Opponent, Card):-
	forall((mayhold(P, Card), P \= Opponent), retract(mayhold(P, Card)))
	% do advanced stuff here to deal with Opponent revealing a card
.

does_not_have(Opponent, Suspect, Weapon, Room):-
	retractall(mayhold(Opponent, Suspect)),
	retractall(mayhold(Opponent, Weapon)),
	retractall(mayhold(Opponent, Room))
.

solved :-
	solvedSuspect,
	solvedWeapon,
	solvedRoom
.

solvedSuspect :-
	findall(S, (suspect(S),mayhold(envelope,S)), Slist),
	length(Slist, 1)
.

solvedWeapon :-
	findall(W, (weapon(W),mayhold(envelope,W)), Wlist),
	length(Wlist, 1)
.

solvedRoom :-
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
				), nl
.

solutions:-
	writeln('Possible cards in envelope.'),
	write('-Suspects: '),
	forall((suspect(S), mayhold(envelope, S)), (write(S), write(' '))),
	nl,
	write('-Weapons: '),
	forall((weapon(W), mayhold(envelope, W)), (write(W), write(' '))),
	nl,
	write('-Rooms: '),
	forall((room(R), mayhold(envelope, R)), (write(R), write(' '))),
	nl
	.

printOppHands :-
	writeln('Opponents\' hands'),
	forall((player(P), P \= envelope),
				 (write(P), write(' holds '), forall(holds(P,C), (write(C), write(' '))), nl)
				)
.

giveSuggestion :-
	suspect(S), mayhold(envelope,S),
	weapon(W), mayhold(envelope,W),
	room(R), mayhold(envelope,R),
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

myTurn :-
	(solved ->
		(writeln('make accusation'),
		solutions
		);
		writeln('Would you like a hint, or will you make a suggestion? Type \'hint\' or \'make\'. Or type \'notes\' to show the notebook'),
		read(Suggest),
		( Suggest == notes ->
			(notebook, myTurn)
		;	Suggest == hint ->
			giveSuggestion
		; Suggest == make ->
			suggest
		; writeln('Invalid input, let\'s try again'),
			myTurn
		)
	)
.

oppTurn :-
	writeln('What cards did this player suggest?'),
	( write('Enter suspect: '), read(S), suspect(S),
	  write('Enter weapon: '), read(W), weapon(W),
	  write('Enter room: '), read(R), room(R),
	  write('Was a card shown this turn? Enter \'yes\' or \'no\' (and don\'t forget the period after!)'),
		read(Input),
	  ( Input == yes ->
	    write('Who showed this card?'), read(Opponent),
			retractall(mayhold(Opponent,S)),
			asserta(mayhold(Opponent,S)),
			retractall(mayhold(Opponent,W)),
			asserta(mayhold(Opponent,W)),
			retractall(mayhold(Opponent,R)),
			asserta(mayhold(Opponent,R)),
			oppTurnNoShow(S, W, R)
	  ; Input == no ->
			writeln('Oh no, your opponent won. Better luck next time!'),
			write('The answer is: '),
			write(S), write(' with a '),
			write(W), write(' in the '),
			write(R), nl
	  ; write('Invalid answer, try again.'),
	    oppTurn
	  )
	; writeln('Invalid card, try again.'),
	  oppTurn
	)
.

oppTurnNoShow(S, W, R):-
	playercount(PCount),
	MAX is PCount-1,
	nl,
	writeln('How many players no counting yourself didn\'t have a card to show?'),
	write('valid inputs [0-'), write(MAX), write(']: '),
	read(Number),
	(	Number == 0 ->
		write('Nothing learned from this')
	;	(Number =< MAX, Number > 0 )->
		oppTurnNoShowLoop(S, W, R, Number)
	;	writeln('Number of players out of range, try again!'),
	oppTurnNoShow(S, W, R)
	)
.

oppTurnNoShowLoop(_, _, _, 0):- writeln('Done with this opponents turn').
oppTurnNoShowLoop(S, W, R, X):-
	nl,
	write('Name a player that had no cards to show'),
	read(Name),
	( player(Name) ->
		does_not_have(Name, S, W, R),
		write('crossed out '),
		write(S), write(', '),
		write(W), write(' and '),
		write(R), write(' '),
		write(' from '),
		write(Name),
		write('\'s column'),
		nl
	;	writeln('no such player, try again!'),
		oppTurnNoShowLoop(S, W, R, X)
	),
	Y is X-1,
	oppTurnNoShowLoop(S, W, R, Y)
.



/* Utility */
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic weapon/1.
