:- use_module(whodunnit).
:- use_module(sudoku).
:- use_module(sudoku_helpers).

main :-
	writeln('--- Who killed Alice? ---'),
	(killer(alice, Killer, Interval) ->
	format('Killer: ~w during ~w~n', [Killer, Interval]);
	writeln('No unique killer found.')),
	writeln('--- Solving Sudoku ---'),
	% Known solvable puzzle (easy)
	% Unsolved = [
	% 	[5, 3, _, _, 7, _, _, _, _],
	% 	[6, _, _, 1, 9, 5, _, _, _],
	% 	[_, 9, 8, _, _, _, _, 6, _],
	% 	[8, _, _, _, 6, _, _, _, 3],
	% 	[4, _, _, 8, _, 3, _, _, 1],
	% 	[7, _, _, _, 2, _, _, _, 6],
	% 	[_, 6, _, _, _, _, 2, 8, _],
	% 	[_, _, _, 4, 1, 9, _, _, 5],
	% 	[_, _, _, _, 8, _, _, 7, 9]],
	Unsolved = [
		[8, _, _, _, _, 1, _, _, _],
		[_, 7, _, 9, _, _, _, 4, _],
		[_, _, 9, _, 7, 8, 3, 2, 5],
		[3, _, 1, _, 9, _, _, 5, _],
		[_, _, 6, _, _, _, 1, _, _],
		[_, 9, _, _, 3, _, 6, _, 2],
		[2, 8, 3, 6, 5, _, 7, _, _],
		[_, 1, _, _, _, 2, _, 8, _],
		[_, _, _, _, 1, _, _, _, 9]],
	(sudoku(Unsolved, Solved) ->
	writeln('Solved Sudoku:'),
		print_sudoku(Solved);
	writeln('No solution found.')).