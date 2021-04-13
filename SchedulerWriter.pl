% CPSC 312 2021
% Some simple Prolog examples. In public domain.
% :- include('Schedule.pl').
:- include('algorithm.pl').
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

assigned('test', slot(date(4,6,2021), range(am(12,0), am(12,30)))).

% if asssigned slots are stored in KB like: assigned(TName, slot(Date, Range)) then to make all cells in output:
% create cells asserts in KB cell(TName, Slot), where TName is the task name assigned to Slot or '' if no task assigned
createCellsWrapper :-
	retractall(cell(_,_,_)),
	planstart(Date),
	createCells(Date).

createCells(Date) :- planend(Date), createCellsHelper(am(12,0), Date).
createCells(Date) :-
	\+ planend(Date),
	createCellsHelper(am(12,0), Date),
	nextDay(Date, ND),
	createCells(ND).

createCellsHelper(Time, Date) :-
	beforeTime(Time, pm(11,30)),
	assigned(TName, slot(Date, range(Time, End))), timeAfter30(Time, End),
	assert(cell(TName, Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date).

createCellsHelper(Time, Date) :-
	beforeTime(Time, pm(11,30)),
	\+ assigned(TName, slot(Date, range(Time, End))), timeAfter30(Time, End),
	assert(cell('', Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date).

createCellsHelper(pm(11,30), Date) :-
	assigned(TName, slot(Date, range(pm(11,30), End))),
	assert(cell(TName, Date, range(pm(11,30), End))).

createCellsHelper(pm(11,30), Date) :-
	\+ assigned(TName, slot(Date, range(pm(11,30), End))),
	assert(cell('', Date, range(pm(11,30), End))).

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
		createTaskRows30(am(12,0), TaskRows).

%Just appending row onto all the dates and returns it
createTopRow(FactRow) :-
		getPlanDatesC(Dates),
		append(['row'], Dates, Row1),
		FactRow =.. Row1.

%need to make end when at time 23:45
createTaskRows15(pm(11,45), [Fact1]) :- 
	planstart(Date),
	createTaskList(pm(11,45), Date, Tasks),
	createTaskRow(pm(11,45), Tasks, Fact1).

createTaskRows15(Time, [Fact1|Facts]) :-
	planstart(Date), beforeTime(Time, pm(11,45)),
	createTaskList(Time, Date, Tasks),
	createTaskRow(Time, Tasks, Fact1),
	timeAfter30(Time, Time2),
	createTaskRows15(Time2, Facts).

createTaskRows30(pm(11,30), [Fact1]) :- 
	planstart(Date),
	createTaskList(pm(11,30), Date, Tasks),
	createTaskRow(pm(11,30), Tasks, Fact1).

createTaskRows30(Time, [Fact1|Facts]) :-
	planstart(Date), beforeTime(Time, pm(11,30)),
	createTaskList(Time, Date, Tasks),
	createTaskRow(Time, Tasks, Fact1),
	timeAfter30(Time, Time2),
	createTaskRows30(Time2, Facts).

createTaskRow(Time, Tasks, FactRow) :-
		timeConvert(Time, CTime),
		append(['row', CTime], Tasks, TaskRow),
		FactRow =.. TaskRow.

% end when Date 2 is planend(Date), must account for a blank cell
createTaskList(Time, Date, [TName]) :- %changequotes to TName
	cell(TName, Date, range(Time, End)),
	planend(Date).
createTaskList(Time, Date, [TName|Tasks]) :-
	cell(TName, Date, range(Time, End)),
	\+ planend(Date),
	nextDay(Date, Date2),
	createTaskList(Time, Date2, Tasks).

write :-
	createCellsWrapper,
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