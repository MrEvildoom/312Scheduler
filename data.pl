% CPSC 312 Project
% Task Scheduler
% Brendan Woodward, David Liu, Jack Heidal
:- include('datetime.pl').
:- use_module(library(csv)).
:- dynamic task/1, due/3, duration/2, prequisite/2, available/2, event/1, start/2, end/2, planstart/1, planend/1.

% loads files from user given location
load :-
    retractFacts,
    write('Task File name: '), flush_output(current_output),
    read_line(TFile),
    csv_read_file(TFile, Tasks, [functor(task), arity(5), skip_header('Task Name')]),
    write('Profile File name: '), flush_output(current_output),
    read_line(PFile),
    csv_read_file(PFile, Profile, [functor(avail), arity(3), skip_header('Date (MM/DD/YYYY)')]),
    readTop2(PFile, StartD, EndD),
    write('Event File name: '), flush_output(current_output),
    read_line(BFile),
    csv_read_file(BFile, Events, [functor(event), arity(4), skip_header('Busy Event')]),
    write('Loading Tasks, Profile, and Events...\n'), flush_output(current_output),
    createTasks(Tasks, TFacts),
    assertFacts(TFacts),
    createProfile(Profile, PFacts),
    assertFacts(PFacts),
    createStartEnd(StartD, EndD, PLFacts),
    assertFacts(PLFacts),
    createEvents(Events, EFacts),
    assertFacts(EFacts),
    write('Success!\n'), flush_output(current_output).

% loads files without the user neeeding to input location
auto_load :-
    retractFacts,
    csv_read_file('tasks.csv', Tasks, [functor(task), arity(5), skip_header('Task Name')]),
    csv_read_file('profile.csv', Profile, [functor(avail), arity(3), skip_header('Date (MM/DD/YYYY)')]),
    readTop2('profile.csv', StartD, EndD),
    csv_read_file('busy.csv', Events, [functor(event), arity(4), skip_header('Busy Event')]),
    write('Loading Tasks, Profile, and Events...\n'), flush_output(current_output),
    createTasks(Tasks, TFacts),
    assertFacts(TFacts),
    createProfile(Profile, PFacts),
    assertFacts(PFacts),
    createStartEnd(StartD, EndD, PLFacts),
    assertFacts(PLFacts),
    createEvents(Events, EFacts),
    assertFacts(EFacts),
    write('Success!\n'), flush_output(current_output).

% createTasks iterates through the tasks and formats them into proper facts ie. due('Quiz', date(04, 10, 2021), am(11, 30))
createTasks([],[]).
createTasks([task(Name, Date, Time, Dur, Pre)|Tasks], [NFact, DTFact, DRFact, PRFact|Facts]) :-
    NFact =.. ['task', Name],
    convertDate(Date, NewDate),
    convertTime(Time, NewTime),
    DTFact =.. ['due', Name, NewDate, NewTime],
    term_to_atom(NewDur, Dur),
    DRFact =.. ['duration', Name, NewDur],
    PRFact =.. ['prequisite', Name, Pre],
    createTasks(Tasks, Facts).

% createProfile iterates through the profile and formats them into propper facts ie. available(date(20, 04, 2021), range(am(10,00), pm(2,5)))
createProfile([],[]).
createProfile([avail(Date, Day, Times)|Dates], Facts) :-
    atomic_list_concat(TList, ',', Times),
    convertTimeRange(TList, AvailRanges),
    convertDate(Date, NewDate),
    createAvails(NewDate, AvailRanges, FactR),
    createProfile(Dates, RestF),
    append(FactR, RestF, Facts).
createProfile([avail(Date, Day, '')|Dates], Facts) :-
    createProfile(Dates, Facts).

% createAvails itearates through ranges for a data and creates all facts for that date ie. available(date(20, 04, 2021), range(am(10,00), pm(2,5)))
createAvails(_,[],[]).
createAvails(Date, [Range|Ranges], [Fact|Facts]) :-
    Fact =.. ['available', Date, Range],
    createAvails(Date, Ranges, Facts).

% convertTimeRange takes a lsit of time ranges and returns a list formatted as range(am(11,00) ,pm(1,00))
convertTimeRange([],[]).
convertTimeRange([TimeR|Times], [range(StartTime, EndTime)|ListRange]) :-
    term_to_atom(SH:SM-EH:EM, TimeR), term_to_atom(SH:SM, ST), term_to_atom(EH:EM, ET),
    convertTime(ST, StartTime),
    convertTime(ET, EndTime),
    convertTimeRange(Times, ListRange).

% createEvents iterates through the events and formats them into propper facts ie. start('Practice', date(04, 12, 2021), pm(4, 45))
createEvents([],[]).
createEvents([event(Name, Date, Start, End)|Events], [NFact, SFact, EFact|Facts]) :-
    NFact =.. ['event', Name],
    convertDate(Date, NewDate),
    convertTime(Start, NewStart),
    convertTime(End, NewEnd),
    SFact =.. ['start', NewDate, NewStart],
    EFact =.. ['end', NewDate, NewEnd],
    createEvents(Events, Facts).

% createStartEnd creates facts for the start and end of the plan
createStartEnd(row(_,'Start', StDate), row(_, 'End', EnDate), [StartF, EndF]) :-
    convertDate(StDate, SDate), convertDate(EnDate, EDate),
    StartF =.. ['planstart', SDate],
    EndF =.. ['planend', EDate].

% retractFacts clears the KB of all relevant facts that are to be loaded in
retractFacts :-
    retractall(task(_)), retractall(due(_,_,_)), retractall(duration(_,_)), retractall(prequisite(_,_)),
    retractall(available(_,_)), retractall(planstart(_)), retractall(planend(_)),
    retractall(event(_)), retractall(start(_,_)), retractall(end(_,_)).
    

% assertFacts iterates through a list of facts and asserts them
assertFacts([]).
assertFacts([H|T]) :-
    assert(H),
    assertFacts(T).

% helper function to read from terminal as single quote atom
read_sq(SQ) :-
    read_line(String),
    atom_string(SQ, String).

% helper function to read from terminal as a string
% from https://stackoverflow.com/questions/36797007/output-any-string-input-with-prolog
read_line(String) :-
    current_input(Input),
    read_string(Input, '\n', '\r', End, String).

% reads the top 2 rows of a CSV File
readTop2(File, Row1, Row2) :-
    csv_read_file_row(File, Row1, [line(Num)]),
    Num is 1,
    csv_read_file_row(File, Row2, [line(Num2)]),
    Num2 is Num + 1.


