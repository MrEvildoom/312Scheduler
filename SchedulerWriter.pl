% CPSC 312 2021
% Some simple Prolog examples. In public domain.
% :- include('Schedule.pl').
:- include('scheduler.pl').
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
getPlanDatesC([CDate|Dates]) :-
		planstart(Date),
		convertDate(CDate, Date),
		nextDay(Date, Date2),
		getPlanDatesHelpC(Date2, Dates).

getPlanDatesHelpC(Date, [CDate]) :- planend(Date), convertDate(CDate, Date).
getPlanDatesHelpC(Date, [CDate|Dates]) :-
		nextDay(Date, Date2),
		\+ planend(Date),
		convertDate(CDate, Date),
		getPlanDatesHelpC(Date2, Dates).

getPlanDates([Date|Dates]) :-
		planstart(Date),
		nextDay(Date, Date2),
		getPlanDatesHelp(Date2, Dates).

getPlanDatesHelp(Date, [Date]) :- planend(Date).
getPlanDatesHelp(Date, [Date|Dates]) :-
		nextDay(Date, Date2),
		\+ planend(Date),
		getPlanDatesHelp(Date2, Dates).

writeToCSV :-
	createAllRows(Rows),
	csv_write_file('out.csv', Rows).

createAllRows([TopRow|TaskRows]) :-
		createTopRow(TopRow),
		createTaskRows(am(12,0), TaskRows).

%Just appending row onto all the dates and returns it
createTopRow(FactRow) :-
		getPlanDatesC(Dates),
		append(['row'], Dates, Row1),
		FactRow =.. Row1.

%need to make end when at time 23:45
createTaskRows(pm(11,45), [Fact1]) :- 
	planstart(Date),
	createTaskList(pm(11,45), Date, Tasks),
	createTaskRow(pm(11,45), Tasks, Fact1).

createTaskRows(Time, [Fact1|Facts]) :-
	planstart(Date), beforeTime(Time, pm(11,45)),
	createTaskList(Time, Date, Tasks),
	createTaskRow(Time, Tasks, Fact1),
	timeAfter15(Time, Time2),
	createTaskRows(Time2, Facts).

createTaskRow(Time, Tasks, FactRow) :-
		timeConvert(Time, CTime),
		append(['row', CTime], Tasks, TaskRow),
		FactRow =.. TaskRow.

% end when Date 2 is planend(Date), must account for a blank cell
createTaskList(Time, Date, ['']) :- 
	%assigned(Task1, slot(Date, range(Time, End))), 
	planend(Date).
createTaskList(Time, Date, [''|Tasks]) :-
		%assigned(Task1, slot(Date, range(Time, End))), % correct to way jack has assigned tasks
		\+ planend(Date),
		nextDay(Date, Date2),
		createTaskList(Time, Date2, Tasks).

write :-
	createAllRows(Rows),
	csv_write_file("output.csv", Rows).

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