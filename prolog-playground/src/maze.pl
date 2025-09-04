:- module(maze, [find_food/2, demo/0]).

% Maze module: BFS shortest path and ASCII rendering

% Example maze layout (5x5) where blocked(X,Y) are walls.
blocked(2,1).
blocked(2,2).
blocked(4,3).
blocked(3,3).

% food position
food(5,5).

% Valid move: up/down/left/right inside bounds and not blocked
in_bounds(X,Y) :- X >= 1, X =< 5, Y >= 1, Y =< 5.
passable(X,Y) :- in_bounds(X,Y), \+ blocked(X,Y).

move((X,Y), (NX,Y)) :- NX is X+1, passable(NX,Y).
move((X,Y), (NX,Y)) :- NX is X-1, passable(NX,Y).
move((X,Y), (X,NY)) :- NY is Y+1, passable(X,NY).
move((X,Y), (X,NY)) :- NY is Y-1, passable(X,NY).

% find_food(+Start, -Path)
% Breadth-first search for shortest path; returns path from Start to food inclusive.
find_food(Start, Path) :-
	food(FX,FY),
	bfs([[Start]], (FX,FY), RevPath),
	reverse(RevPath, Path).

% bfs(+Queue, +Goal, -Path)
% Queue is list of paths (each path is list of positions, head is current node)
bfs([[Goal|RestPath]|_], Goal, [Goal|RestPath]).
bfs([CurrentPath|OtherPaths], Goal, Result) :-
	CurrentPath = [Node|_],
	findall([Next|CurrentPath], (move(Node, Next), \+ member(Next, CurrentPath)), NewPaths),
	append(OtherPaths, NewPaths, Queue2),
	bfs(Queue2, Goal, Result).

demo :-
	Start = (1,1),
	(find_food(Start, Path) ->
		format('Found path from ~w to food: ~w~n', [Start, Path]);
		(writeln('No path found'), Path = [])),
	% show food location
	food(FX,FY), format('Food at: (~w,~w)~n', [FX,FY]),
	% render ASCII maze with path overlay
	render_maze(Start, Path).


% render_maze(+Start, +Path)
% Prints the maze as ASCII art. Coordinates are (X,Y) with X=1..5, Y=1..5.
% Rows are printed top-to-bottom (Y=5 down to Y=1) so the output is readable for slides.
render_maze(Start, Path) :-
	writeln('Maze legend: # = wall, . = empty, S = start, F = food, * = path'),
	render_rows(5, Start, Path).

render_rows(0, _Start, _Path) :- !.
render_rows(Y, Start, Path) :-
	Y > 0,
	render_row(1, Y, Start, Path),
	NY is Y - 1,
	render_rows(NY, Start, Path).

render_row(X, _Y, _Start, _Path) :- X > 5, !, nl.
render_row(X, Y, Start, Path) :-
	X =< 5,
	cell_char(X, Y, Start, Path, Ch),
	format('~w', [Ch]),
	NX is X + 1,
	render_row(NX, Y, Start, Path).

cell_char(X, Y, Start, Path, '#') :- blocked(X, Y), !.
cell_char(X, Y, Start, _Path, 'S') :- Start = (X, Y), !.
cell_char(X, Y, _Start, _Path, 'F') :- food(X, Y), !.
cell_char(X, Y, _Start, Path, '*') :- member((X, Y), Path), !.
cell_char(_X, _Y, _Start, _Path, '.') :- !.
% Rows are printed top-to-bottom (Y=5 down to Y=1) so the output is readable for slides.
