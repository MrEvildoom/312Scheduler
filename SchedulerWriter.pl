% CPSC 312 2021
% Some simple Prolog examples. In public domain.
:- include('Schedule.pl', 'scheduler.pl')
:- use_module(library(csv)).

% To load in Prolog, at the ?- prompt:
% swipl
% ?- [write].

%map_item(P, row(Name, Date, range(S, E))) :-
%    P =.. [_, Date, Name, atom_concat(S, '-', E)].

%map_item(P, row(Name, Date, Range)) :-
%    P =.. [_, Date, Name, Range].

%map_item(P, row(Name, Date, range(S, E))) :-
%	atomic_concat(S, ':' ,L),
%	atomic_concat(L, ':' ,R),
%   P =.. [_, Date, Name, R].

%map_item(P, row(Name, Date, Range)) :-
	%atomic_concat('', ':', E),
%    P =.. [_, Date, Name, E].

%insert(File, Data) :-
    %maplist(map_item, Data, Rows),
%    csv_write_file(File, Data, [separator(0',)]).

%TODO on all convert times and dates to be atoms (can use conversions in Schedule.pl)
getPlanDates([Date|Dates]) :-
		planstart(Date),
		nextDay(Date, Date2),
		getPlanDatesHelp(Date2, Dates).

getPlanDatesHelp(Date, [Date|Dates]) :- planend(Date).
getPlanDatesHelp(Date, [Date|Dates]) :-
		nextDay(Date, Date2),
		getPlanDatesHelp(Date2, Dates).

writeToCSV :-
	createAllRows(Rows),
	csv_write_file('out.csv', Rows).

createAllRows([TopRow|TaskRows]) :-
		createTopRow(TopRow),
		createTaskRows(TaskRows).

%Just appending row onto all the dates and returns it
createTopRow(FactRow) :-
		getPlanDates(Dates),
		append(['row'], AllDates, Row1),
		FactRow =.. Row1.

%need to make end when at time 23:45
createTaskRows(pm(11,45), [Fact1|Facts]) :- 
	planstart(Date),
	createTaskList(Time, Date, Tasks),
	createTaskRow(Time, Tasks, Fact1).

createTaskRows(Time, [Fact1|Facts]) :-
	planstart(Date),
	createTaskList(Time, Date, Tasks),
	createTaskRow(Time, Tasks, Fact1),
	timeAfter15(Time, Time2),
	createTaskRows(Time2, Facts).

createTaskRow(Time, Tasks, FactRow) :-
		append(['row', Time], Tasks, TaskRow),
		FactRow =.. TaskRow.

% end when Date 2 is planend(Date), must account for a blank cell
createTaskList(Time, Date, [Task1|Tasks]) :- assigned(Task1, slot(Date, range(Time, End))), planend(Date).
createTaskList(Time, Date, [Task1|Tasks]) :-
		assigned(Task1, slot(Date, range(Time, End))), % correct to way jack has assigned tasks
		nextDay(Date, Date2),
		createTaskList(Time, Date2, Tasks).


% insert("output.csv",[test(name, a, b, c), test(name, d, e, f), test(name, g, h, c)], test(name, g, m, i)]).
% insert("output1.csv",[test(a, b, c1), test(d, e, f)]). %THIS WORKS

% insert("output.csv",[scheduledTask("Work on 312 assignment", "4/6/2021", "12:00")]).
						
% insert("output.csv",[scheduledTask("Work on 312 assignment", "4/6/2021", range(1200, 5000))]).

%insert("output.csv",[	scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()),  
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range()), 
%						scheduledTask(name, date, range(12,14))]).