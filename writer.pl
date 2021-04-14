%% File to write the schedule results to a CSV file %%
:- include('algorithm.pl').

% assigned('test', slot(date(4,6,2021), range(am(12,0), am(12,30)))).

% if asssigned slots are stored in KB like: assigned(TName, slot(Date, Range)) then to make all cells in output:
% create cells asserts in KB cell(TName, Slot), where TName is the task name assigned to Slot or '' if no task assigned
createCellsWrapper :-
	retractall(cell(_,_,_)),
	planstart(Date),
	createCells(Date, Cells),
	maplist(assert, Cells).

createCells(Date, Cells) :- planend(Date), createCellsHelper(am(12,0), Date, Cells).
createCells(Date, AllCells) :-
	\+ planend(Date),
	createCellsHelper(am(12,0), Date, Cells1),
	nextDay(Date, ND),
	append(Cells1, Cells, AllCells),
	createCells(ND, Cells).

createCellsHelper(Time, Date, [cell(TName, Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	assigned(TName, slot(Date, range(Time, End))), timeAfter30(Time, End),
	%assert(cell(TName, Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

createCellsHelper(Time, Date, [cell('', Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	\+ assigned(_, slot(Date, range(Time, End))), timeAfter30(Time, End),
	\+ duringEvent(Date, Time,_),
	%assert(cell('', Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

createCellsHelper(Time, Date, [cell(EName, Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	\+ assigned(_, slot(Date, range(Time, End))), timeAfter30(Time, End),
	duringEvent(Date, Time, EName),
	%assert(cell('', Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

createCellsHelper(pm(11,30), Date, [cell(TName, Date, range(pm(11,30), End))]) :-
	assigned(TName, slot(Date, range(pm(11,30), End))).
	%assert(cell(TName, Date, range(pm(11,30), End))).

createCellsHelper(pm(11,30), Date, [cell('', Date, range(pm(11,30), End))]) :-
	\+ assigned(_, slot(Date, range(pm(11,30), End))),
	\+ duringEvent(Date, pm(11,30),_).
	%assert(cell('', Date, range(pm(11,30), End))).

createCellsHelper(pm(11,30), Date, [cell(EName, Date, range(pm(11,30), End))]) :-
	\+ assigned(_, slot(Date, range(pm(11,30), End))),
	duringEvent(Date, pm(11,30), EName).

% gets plan dates converted to SQ
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

%Gets plan dates in date(MM, DD, YYYY) format
getPlanDates([Date|Dates]) :-
		planstart(Date),
		nextDay(Date, Date2),
		getPlanDatesHelp(Date2, Dates).

getPlanDatesHelp(Date, [Date]) :- planend(Date).
getPlanDatesHelp(Date, [Date|Dates]) :-
		nextDay(Date, Date2),
		\+ planend(Date),
		getPlanDatesHelp(Date2, Dates).

%Creates all rows for CSV writing
createAllRows([TopRow|TaskRows]) :-
		createTopRow(TopRow),
		createTaskRows30(am(12,0), TaskRows).

%Just appending row onto all the dates and returns it
createTopRow(FactRow) :-
		getPlanDatesC(Dates),
		append(['row'], Dates, Row1),
		FactRow =.. Row1.

%creates task rows for 15 min intervals OLD
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

% creates task rows for 30 min intervals
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

% creates the task row for the given time
createTaskRow(Time, Tasks, FactRow) :-
		timeConvert(Time, CTime),
		append(['row', CTime], Tasks, TaskRow),
		FactRow =.. TaskRow.

% creates a list of tasks on a given time cycling through all dates (until planend)
createTaskList(Time, Date, [TName]) :- 
	cell(TName, Date, range(Time,_)),
	planend(Date).
createTaskList(Time, Date, [TName|Tasks]) :-
	cell(TName, Date, range(Time,_)),
	\+ planend(Date),
	nextDay(Date, Date2),
	createTaskList(Time, Date2, Tasks).

%writes results to CSV file
write :-
	createCellsWrapper,
	createAllRows(Rows),
	csv_write_file("output.csv", Rows).

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