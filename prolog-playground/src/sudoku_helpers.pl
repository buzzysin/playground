:-module(sudoku_helpers, [print_sudoku/1]).

% print_sudoku(+Board)
% Nicely prints a 9x9 Sudoku board with box-drawing separators between 3x3 blocks.
print_sudoku(Board) :-
	forall(nth0(R, Board, Row),
		(print_row_string(Row, Line, S1, S2, S3),
			writeln(Line),
			(R mod 3 =:= 2,
				R \= 8 ->
	string_length(S1, L1),
				string_length(S2, L2),
				string_length(S3, L3),
				make_sep_from_lengths(L1, L2, L3, Sep),
				writeln(Sep);
	true))).

% print_row_string(+Row, -Line, -S1, -S2, -S3)
print_row_string(Row, Line, S1, S2, S3) :-
	findall(CellStr,
		(member(Cell, Row),
			cell_to_string(Cell, CellStr)),
		Strs),
	group3(Strs, [G1, G2, G3]),
	atomic_list_concat(G1, ' ', S1),
	atomic_list_concat(G2, ' ', S2),
	atomic_list_concat(G3, ' ', S3),
	atomic_list_concat([S1, ' │ ', S2, ' │ ', S3], Line).

% make_sep_from_lengths(+L1, +L2, +L3, -Sep)
make_sep_from_lengths(L1, L2, L3, Sep) :-
	L1 >= 0,
	L2 >= 0,
	L3 >= 0,
	make_dashes(L1, D1),
	make_dashes(L2, D2),
	make_dashes(L3, D3),
	% use '─┼─' between groups so separators include the spaces around the vertical bars
	atomic_list_concat([D1, '─┼─', D2, '─┼─', D3], Sep).

% make_dashes(+N, -D)
make_dashes(N, D) :-
	N > 0,
	length(Chars, N),
	maplist((=('─')),
		Chars),
	atomic_list_concat(Chars, '', D).
make_dashes(0, '').

% cell_to_string(+Cell, -Str)
cell_to_string(Cell, Str) :-
	(var(Cell) ->
	Str = '_';
	number(Cell) ->
	number_string(Cell, Str);
	atom_string(Cell, Str)).

% Same as above, but uses an ANSI escape code for background colouring

pretty_cell_to_string(Cell, Str) :-
	var(Cell) ->
	Str = ' . ';
	number(Cell) ->
	pretty_number_to_string(Cell, Str);
	atom_string(Cell, Str).

	% (var(Cell) ->
	% Str = ' . ';
	% number(Cell) ->
	% format(string(Str), ' ~w ', [Cell]);
	% atom_string(Cell, Str)).

% 1 - white on red
% 2 - black on yellow
% 3 - black on green
% 4 - white on blue
% 5 - black on cyan
% 6 - black on magenta
% 7 - white on gray
% 8 - black on white
% 9 - black on blue
pretty_number_to_string(Number, Str) :-
	(Number =:= 1 ->
	format(string(Str),
			'\e[97;041m ~w \e[0m',
			[Number]);
	Number =:= 2 ->
	format(string(Str),
			'\e[30;043m ~w \e[0m',
			[Number]);
	Number =:= 3 ->
	format(string(Str),
			'\e[30;042m ~w \e[0m',
			[Number]);
	Number =:= 4 ->
	format(string(Str),
			'\e[97;044m ~w \e[0m',
			[Number]);
	Number =:= 5 ->
	format(string(Str),
			'\e[30;046m ~w \e[0m',
			[Number]);
	Number =:= 6 ->
	format(string(Str),
			'\e[30;045m ~w \e[0m',
			[Number]);
	Number =:= 7 ->
	format(string(Str),
			'\e[97;047m ~w \e[0m',
			[Number]);
	Number =:= 8 ->
	format(string(Str),
			'\e[30;107m ~w \e[0m',
			[Number]);
	Number =:= 9 ->
	format(string(Str),
			'\e[30;104m ~w \e[0m',
			[Number]);
	format(string(Str),
			' ~w ',
			[Number])).

% group3(+List9, -Groups3)
group3([A, B, C, D, E, F, G, H, I], [[A, B, C], [D, E, F], [G, H, I]]).