/*
 * Author  : Jakub Šoustar <jakub.soustar@gmail.com> <xsoust02@stud.fit.vutbr.cz>
 * Project : Turing Machine
 *
*/

/*
 * MAIN
*/

% Set to true to also print runtime in milliseconds.
do_profile :- false.

start :-
	% Hide the default prompt.
	prompt(_, ''),
	% Read all lines from stdin.
	read_lines(Lines),
	% Get transitions and the initial tape.
	get_transitions(Lines, Transitions),
	get_tape(Lines, Tape),
	% Initialization done, cut.
	!,
	(	% Run the simulation.
		run(Tape, Transitions, Configurations),
		% Print the chain of configurations leading to the final state.
		write_configurations(Configurations)
	;	% Abnormal termination.
		writeln('Simulation has abnormally terminated. Possible reasons are:'),
		writeln(' * Attempted to move the head to the left from the first cell'),
		writeln(' * Final state could not be reached')
	),
	!,
	(	do_profile,
		statistics('runtime', [_, CPUTime]),
		writeln(CPUTime)
	;	true
	),
	halt.

/*
 * END MAIN
*/

/*
 * TURING MACHINE
*/

% Simulate a Turing machine using provided tape and transition function.
run(Tape, Transitions, Configurations) :-
	Configurations = [Tape | NewConfigurations],
	configuration(Tape, State, Symbol),
	(	State == 'F'
		% Done
	->	NewConfigurations = []
	;	(	% Find a possible transition.
			transition(Transitions, State, Symbol, Transition)

		;	% No transition. Fail and backtrack.
			!,
			fail
		),
		% Perform the transition.
		action(Tape, Transition, NewTape),
		% Next step.
		run(NewTape, Transitions, NewConfigurations)
	).

% Perform a single action based on the current configuration and selected transition.
action(TapeIn, Transition, TapeOut) :-
	Transition = [_, _, _, NewSymbol],
	(	% Rewrite a symbol under the head.
		is_tape_symbol(NewSymbol),
		symbol(TapeIn, Transition, TapeOut)

	;	% Move the head to the right.
		NewSymbol == 'R',
		right(TapeIn, Transition, TapeOut)

	;	% Move the head to the left.
		NewSymbol == 'L',
		left(TapeIn, Transition, TapeOut)
	).

% Transition into a new state by replacing an empty symbol at the end of the tape.
symbol([OldState], [OldState, _, NewState, NewSymbol], [NewState, NewSymbol]).
% Transition into a new state by replacing a symbol under head.
symbol(TapeIn, Transition, TapeOut) :-
	Transition = [OldState, _, NewState, NewSymbol],
	TapeIn = [Head, Neck | Tail],
	(	Head == OldState,
		% We're at head's position.
		append([NewState, NewSymbol], Tail, TapeOut)

	;	symbol([Neck | Tail], Transition, NewTapeOut),
		% Preserve the beginning of the tape.
		append([Head], NewTapeOut, TapeOut)
	).

% Transition into a new state by moving the head to the right at the end of the tape.
right([OldState], [OldState, _, NewState, _], [' ', NewState]).
% Transition into a new state by moving the head to the right.
right(TapeIn, Transition, TapeOut) :-
	Transition = [OldState, _, NewState, _],
	TapeIn = [Head, Neck | Tail],
	(	% Symbol previously under the head, new head position, and the rest of the tape.
		Head == OldState,
		append([Neck, NewState], Tail, TapeOut)

	;	right([Neck | Tail], Transition, NewTapeOut),
		% Preserve the beginning of the tape.
		append([Head], NewTapeOut, TapeOut)
	).

% End of the tape. Fall off.
left([OldState | _], [OldState, _, _, _], _) :-
	% Just fail. We're relying on prolog's backtracking and printing an error
	% message here would mess up the output.
	fail.
% Transition into a new state by moving the head to the left.
left(TapeIn, Transition, TapeOut) :-
	Transition = [OldState, _, NewState, _],
	TapeIn = [Head, Neck | Tail],
	(	Neck == OldState,
		(	% New head position, symbol previously left of the head. Ignore all the trailing blanks.
			is_blank(Tail),
			TapeOut = [NewState, Head]

		;	% New head position, symbol previously left of the head, and the rest of the tape.
			append([NewState, Head], Tail, TapeOut)
		)

	;	left([Neck | Tail], Transition, NewTapeOut),
		% Preserve the beginning of the tape.
		append([Head], NewTapeOut, TapeOut)
	).

% Select a possible transition from the current configuration.
transition([Head | Tail], State, Symbol, Transition) :-
	Head = [OldState, OldSymbol, NewState, _],
	(	% 1. Try to match transition to the final state to avoid infinity trap.
		OldSymbol == Symbol,
		OldState == State,
		NewState == 'F',
		Transition = Head

	;	% 2. Check rest the of transitions for one leading to the final state (see 1).
		transition(Tail, State, Symbol, Transition)

	;	% 3. Match any viable transition.
		OldSymbol == Symbol,
		OldState == State,
		Transition = Head
	).

% Extract current state and symbol from tape.
configuration([Head | Tail], State, Symbol) :-
	(	is_state_symbol(Head),
		State = Head,
		(	% There is a symbol under the head.
			Tail = [Symbol | _]
		;	% There is always a blank available.
			Symbol = ' '
		)
	;	configuration(Tail, State, Symbol)
	).

/*
 * END TURING MACHINE
*/

/*
 * TRANSITIONS
*/

% Decode transitions from the lines read from stdin.
get_transitions(Lines, Transitions) :-
	% Initial tape is on the last line.
	append(RawTransitions, [_], Lines),
	% Transitions sanitation.
	check_transitions(RawTransitions, Transitions).

% No transitions.
check_transitions([], []).
% Check each transition.
check_transitions([HeadIn | TailIn], [HeadOut | TailOut]) :-
	check_transition(HeadIn, HeadOut),
	check_transitions(TailIn, TailOut).

% Check that transition is properly encoded.
check_transition(TransitionIn, TransitionOut) :-
	TransitionIn = [OldState, ' ', OldSymbol, ' ', NewState, ' ', NewSymbol],
	is_state_symbol(OldState),
	is_state_symbol(NewState),
	is_tape_symbol(OldSymbol),
	(	is_tape_symbol(NewSymbol)
	;	NewSymbol == 'L'
	;	NewSymbol == 'R'
	),
	TransitionOut = [OldState, OldSymbol, NewState, NewSymbol].
% The transition is not properly encoded.
check_transition(Line, _) :-
	write('Invalid transition: '),
	writeln(Line),
	halt.

/*
 * END TRANSITIONS
*/

/*
 * TAPE
*/

% Decode initial tape from the lines read from stdin.
get_tape(Lines, Tape) :-
	% Initial tape is on the last line.
	append(_, [RawTape], Lines),
	% Tape sanitation.
	check_tape(RawTape, ValidTape),
	init_tape(ValidTape, Tape).

% Empty tape.
check_tape([], []).
% Check that tape is properly encoded.
check_tape([HeadIn | TailIn], [HeadOut | TailOut]) :-
	is_tape_symbol(HeadIn),
	HeadOut = HeadIn,
	check_tape(TailIn, TailOut).
% The tape is not properly encoded.
check_tape([Char | _], _) :-
	write('Invalid tape symbol: '),
	writeln(Char),
	halt.

% Initialize a tape by placing start state at it's beginning.
init_tape(TapeIn, TapeOut) :-
	append(['S'], TapeIn, TapeOut).

/*
 * END TAPE
*/

/*
 * OUTPUT
*/

write_configurations([]).
write_configurations([Head | Tail]) :-
	write_tape(Head),
	write_configurations(Tail).

write_tape([]) :-
	write('\n').
write_tape([Head | Tail]) :-
	write(Head),
	write_tape(Tail).

/*
 * END OUTPUT
*/

/*
 * INPUT
*/

% Read list of lines from stdin. List ends with EOF.
read_lines(Lines) :-
	read_line(Line, Char),
	(	Char \== end_of_file,
		read_lines(Rest),
		Lines = [Line | Rest]
	;	Lines = []
	).

% Read single line from stdin. Line ends either with LF or EOF.
read_line(Line, Char) :-
	get_char(Char),
	(	is_eof_or_eol(Char),
		Line = [],
		!
	;	read_line(Rest, _),
		Line = [Char | Rest]
	).

/*
 * END INPUT
*/

/*
 * HELPERS
*/

is_eof_or_eol(Char) :-
	Char == end_of_file;
	(	char_code(Char, Code),
		Code == 10
	).

is_state_symbol(Char) :-
	char_type(Char, upper).

is_tape_symbol(Char) :-
	char_type(Char, lower);
	Char == ' '.

is_blank([' ']).
is_blank([' ' | Tail]) :-
	is_blank(Tail).

/*
 * END HELPERS
*/
