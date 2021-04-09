% CPSC 312 2021
% Some simple Prolog examples. In public domain.
:- use_module(library(csv)).

% To load in Prolog, at the ?- prompt:
% swipl
% ?- [write].

%insert(File, Data) :-
%    Data = [X | Xs],
%    X =.. [_, _, A, B, C],
%    csv_write_file(File, [row(A, B, C)], [separator(0,)]),
%	insert(File, Xs).

%map_item(P, row(Name, Date, range(S, E))) :-
%    P =.. [_, Date, Name, atom_concat(S, '-', E)].

%map_item(P, row(Name, Date, Range)) :-
%    P =.. [_, Date, Name, Range].

%map_item(P, row(Name, Date, range(S, E))) :-
%	atomic_concat(S, ':' ,L),
%	atomic_concat(L, ':' ,R),
%   P =.. [_, Date, Name, R].

map_item(P, row(Name, Date, range(S, E))) :-
	atomic_concat('', ':', E),
    P =.. [_, Date, Name, E].

insert(File, Data) :-
    maplist(map_item, Data, Rows),
    csv_write_file(File, Rows, [separator(0',)]).



% insert("output.csv",[test(name, a, b, c), test(name, d, e, f), test(name, g, h, c)], test(name, g, m, i)]).
% insert("output1.csv",[test(a, b, c1), test(d, e, f)]). %THIS WORKS

insert("output.csv",[scheduledTask("Work on 312 assignment", "4/6/2021", "12:00")]).
						
insert("output.csv",[scheduledTask("Work on 312 assignment", "4/6/2021", range(1200, 5000))]).

%insert("output.csv",[	scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()),  
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range(12,14))]).