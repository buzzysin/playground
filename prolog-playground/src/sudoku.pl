:-module(sudoku, [sudoku/2]).

% column(+Idx, +Arr, -Col)
% Extracts a column from the Sudoku grid
column(Idx, Arr, Col) :-
	findall(RowItem,
		(member(Row, Arr),
			nth0(Idx, Row, RowItem)),
		Col).

% row(+Idx, +Arr, -Row)
% Extracts a row from the Sudoku grid
row(Idx, Arr, Row) :-
	nth0(Idx, Arr, Row). 

% square(+Idx0, +Idx1, +Arr, -Square)
% Extracts the sub-grid (3x3 square) from the Sudoku grid
square(Idx0, Idx1, Arr, Square) :-
	RowStart is (Idx0//3)*3,
	ColStart is (Idx1//3)*3,
	findall(Elem,
		(between(0, 2, ROff),
			RowIdx is RowStart + ROff,
			nth0(RowIdx, Arr, Row),
			between(0, 2, COff),
			% row(+Idx, +Arr, -Row)
			ColIdx is ColStart + COff,
			nth0(ColIdx, Row, Elem)),
		Square).

% unique(+Elems) - Leverages the fact that sort/2 removes duplicates
unique(Elems) :-
	sort(Elems, Unique),
	length(Elems, Len),
	length(Unique, Len).

% sudoku_row(+Idx, +Arr) - True if all elements in the Row are unique
sudoku_row(Idx, Arr) :-
	row(Idx, Arr, Row),
	findall(Elem,
		nth0(_, Row, Elem),
		Elems),
	unique(Elems).
  
% sudoku_column(+Idx, +Arr) - True if all elements in the Col are unique
sudoku_column(Idx, Arr) :-
	column(Idx, Arr, Col),
	findall(Elem,
		nth0(_, Col, Elem),
		Elems),
	unique(Elems).
  
% sudoku_square(+Idx0, +Idx1, +Arr) - True if all elements in the Square are unique
sudoku_square(Idx0, Idx1, Arr) :-
	square(Idx0, Idx1, Arr, Square),
	findall(Elem,
		nth0(_, Square, Elem),
		Elems),
	unique(Elems).

% sudoku(+Arr) - True if Arr is a valid Sudoku grid
sudoku(Arr) :-
	length(Arr, 9),
	forall(between(0, 8, I),
		sudoku_row(I, Arr)),
	forall(between(0, 8, I),
		sudoku_column(I, Arr)),
	forall(between(0, 2, I),
		forall(between(0, 2, J),
			sudoku_square(I, J, Arr))).

% sudoku(+Unsolved, -Solved) - true if Solved is a valid Sudoku solution
sudoku(Unsolved, Solved) :-
	solve(Unsolved, Solved),
	sudoku(Solved).

% solved(+Unsolved, -Solved) - Solved is the same nested list structure as Unsolved, with variables filled.
solve(Unsolved, Solved) :-
	Solved = Unsolved,
	length(Solved, 9),
	maplist(same_length(Solved),
		Solved),
	% collect coordinates of all variables (empty cells)
	findall((R, C),
		(nth0(R, Solved, Row),
			nth0(C, Row, Cell),
			var(Cell)),
		Vars),
	fill_holes(Solved, Vars).

% fill_holes(+Board, -ListOf(R,C)) - assign values to each variable by backtracking
fill_holes(_, []).
fill_holes(Board, [(R, C)|Rest]) :-
	between(1, 9, Val),
	can_place(Board, R, C, Val),
	nth0(R, Board, Row),
	nth0(C, Row, Cell),
	Cell = Val,
	fill_holes(Board, Rest).

% can_place(+Board, +R, +C, +Val) - true if Val doesn't conflict with existing nonvar entries
can_place(Board, R, C, Val) :-
	nth0(R, Board, Row),
	 \+ (member(X, Row),
		nonvar(X),
		X == Val),
	column(C, Board, Col),
	 \+ (member(Y, Col),
		nonvar(Y),
		Y == Val),
	square(R, C, Board, Sq),
	 \+ (member(Z, Sq),
		nonvar(Z),
		Z == Val).
