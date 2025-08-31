:- module(whodunnit, [
    victim/3,
    alibi/4,
    coincident/3,
    killer/3
]).

% --- Database ---
% victim(Person, Place, During).
victim(alice, park, during(10, 12)).

% alibi(Person, Place, Possession, During).
alibi(bob,   park,   knife, during(11, 12)).
alibi(carol, cafe,   book,  during(9, 11)).
alibi(dave,  park,   phone, during(10, 11)).
alibi(erin,  library, pen,  during(8, 13)).
alibi(frank, park,   dog,   during(10, 10.5)).

% --- Rules ---

% coincident(during(A,B), during(C,D), during(Start,End)).
coincident(during(A,B), during(C,D), during(Start,End)) :-
    Start is max(A,C),
    End is min(B,D),
    Start < End.  % real overlap

% killer(Victim, Accused, Interval).
% Accused is killer of Victim if:
%   - They are at the same place.
%   - They overlap in time.
%   - No one else overlaps in that same interval.
killer(Victim, Accused, Interval) :-
    victim(Victim, Place, VDuring),
    alibi(Accused, Place, _, ADuring),
    coincident(VDuring, ADuring, Interval),
    \+ (
        alibi(Other, Place, _, ODuring),
        Other \= Accused,
        coincident(VDuring, ODuring, Interval)
    ).
