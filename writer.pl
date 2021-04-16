%% File to write the schedule results to a CSV file %%
:- include('algorithm.pl').
:- dynamic cell/3.

% true if cells are asserted into the knowledge base where a cell is the name of the task, event, or an 'x' for busy or '' for unscheduled at each tiem of each day
createCellsWrapper :-
	retractall(cell(_,_,_)),
	planstart(Date),
	createCells(Date, Cells),
	maplist(assert, Cells).

% true if AllCells is all cells from Date to the planend
createCells(Date, Cells) :- planend(Date), createCellsHelper(am(12,0), Date, Cells).
createCells(Date, AllCells) :-
	\+ planend(Date),
	createCellsHelper(am(12,0), Date, Cells1),
	nextDay(Date, ND),
	append(Cells1, Cells, AllCells),
	createCells(ND, Cells).


% create cells where a task has been assigned
createCellsHelper(Time, Date, [cell(TName, Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	assigned(TName, slot(Date, range(Time, End))), timeAfter30(Time, End),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

% create cell when there is a slot available for work but no task has been assigned to that slot
createCellsHelper(Time, Date, [cell('', Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	slot(Date, range(Time, End)),
	\+ assigned(_, slot(Date, range(Time, End))), timeAfter30(Time, End),
	\+ duringEvent(Date, Time,_),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

% create cells when no task has been assigned
createCellsHelper(Time, Date, [cell('X', Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	\+ slot(Date, range(Time, End)),
	\+ assigned(_, slot(Date, range(Time, End))), timeAfter30(Time, End),
	% above is probably unnecessary, keeping it in to be safe 
	\+ duringEvent(Date, Time,_),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

% create cells when events have been scheduled
createCellsHelper(Time, Date, [cell(EName, Date, range(Time, End))|Cells]) :-
	beforeTime(Time, pm(11,30)),
	\+ assigned(_, slot(Date, range(Time, End))), timeAfter30(Time, End),
	duringEvent(Date, Time, EName),
	%assert(cell('', Date, range(Time, End))),
	timeAfter30(Time, T30),
	createCellsHelper(T30, Date, Cells).

% create 11:30 - 12:00 cell if a task has been scheduled
createCellsHelper(pm(11,30), Date, [cell(TName, Date, range(pm(11,30), End))]) :-
	assigned(TName, slot(Date, range(pm(11,30), End))).

% create 11:30 - 12:00 cell if no task scheduled
createCellsHelper(pm(11,30), Date, [cell('', Date, range(pm(11,30), End))]) :-
	\+ assigned(_, slot(Date, range(pm(11,30), End))),
	\+ duringEvent(Date, pm(11,30),_).

% create 11:30 - 12:00 cell if event scheduled
createCellsHelper(pm(11,30), Date, [cell(EName, Date, range(pm(11,30), End))]) :-
	\+ assigned(_, slot(Date, range(pm(11,30), End))),
	duringEvent(Date, pm(11,30), EName).

% gets plan dates converted to SQ
getPlanDatesC([CDate|Dates]) :-
		planstart(Date),
		convertDate(CDate, Date),
		nextDay(Date, Date2),
		getPlanDatesHelpC(Date2, Dates).

% converts the dates until plan end
getPlanDatesHelpC(Date, [CDate]) :- planend(Date), convertDate(CDate, Date).
getPlanDatesHelpC(Date, [CDate|Dates]) :-
		nextDay(Date, Date2),
		\+ planend(Date),
		convertDate(CDate, Date),
		getPlanDatesHelpC(Date2, Dates).

% Gets plan dates in date(MM, DD, YYYY) format
getPlanDates([Date|Dates]) :-
		planstart(Date),
		nextDay(Date, Date2),
		getPlanDatesHelp(Date2, Dates).

% gets dates until plan end
getPlanDatesHelp(Date, [Date]) :- planend(Date).
getPlanDatesHelp(Date, [Date|Dates]) :-
		nextDay(Date, Date2),
		\+ planend(Date),
		getPlanDatesHelp(Date2, Dates).

% Creates all rows for CSV writing
createAllRows([TopRow|TaskRows]) :-
		createTopRow(TopRow),
		createTaskRows30(am(12,0), TaskRows).

% Just appending row onto all the dates and returns it
createTopRow(FactRow) :-
		getPlanDatesC(Dates),
		append(['row', ''], Dates, Row1),
		FactRow =.. Row1.

% creates task rows for 15 min intervals OLD
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
writeToCSV :-
	once(createCellsWrapper),
	once(createAllRows(Rows)),
	csv_write_file("output.csv", Rows).