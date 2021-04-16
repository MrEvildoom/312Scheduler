:- include('writer.pl').
:- discontiguous executeChosenMethod/1.

% main funtion that allows user to upload files, read facts, scehdulke tasks, and output the file.
mainf :- 
    retractall(assigned(_,_)),
    write('Please make sure you have uploaded a valid profile, tasks file, and busy times file! \n Press Enter when ready.\n'), flush_output(current_output),
    read_sq(_),
    catch(load, _, recoverLoad),
    askForInfo, 
    getMaxTime,
    write('Creating a schedule for you...\n'), flush_output(current_output),
    once(assertSchedule),
    write('Schedule created, writing schedule to CSV...\n'), flush_output(current_output),
    writeToCSV,
    write('Schedule written to output.csv!\n'), flush_output(current_output).

getMaxTime :-
    write('Do you want to schedule breaks after 2 hours? (or no breaks) (y/n)\n'), flush_output(current_output),
    read_sq(YorN),
    (isYes(YorN) -> 
    (changeMaxTime);
    (true)).

changeMaxTime :-
    retractall(max_time(_)), assert(max_time(4)).

% recovers the load from a catch and redoes load until it works
recoverLoad :-
    write('One of the given profile, tasks file, or busy times file is invalid\n'), flush_output(current_output),
    write('Please upload a valid file for each 3! Press Enter when you are ready.\n'), flush_output(current_output),
    read_sq(_),
    catch(load,_,  recoverLoad).

% gets a y or n form user and loops until true
askForInfo :-
    write('Would you like to know some more info about your profile? (y/n) \n'), flush_output(current_output),
    read_sq(YesOrNo),
    checkYes(YesOrNo) -> processQuestions(YesOrNo); 
                    (write('Invalid response. Please answer either yes (y) or no (n) '), askForInfo).

%true if y or n
checkYes(y).
checkYes(n).

%true if yes
isYes(y).

% if n proceed, if y process user input to answer questions about files.
processQuestions(n) :-
    write('Understood. Moving forward to create a schedule.\n'), flush_output(current_output).
processQuestions(y) :-
    write('What would you like to know?\n'), flush_output(current_output),
    write('(1) Show me info for task X \n'), flush_output(current_output), %done
    write('(2) Show me info for event X? \n'), flush_output(current_output), %done B
    write('(3) When am I available on X date? \n'), flush_output(current_output), %done B
    write('(4) Which tasks require at least X hours? \n'), flush_output(current_output), %done
    write('(5) What task(s) are due on day X \n'), flush_output(current_output),        %done
    write('(6) What days am I available at time X? \n'), flush_output(current_output),	%done B
    write('(7) Which tasks have a prerequisite? \n'), flush_output(current_output),
    write('(8) What is my shortest task? \n'), flush_output(current_output),            %done
    write('(9) What is my longest task? \n'), flush_output(current_output),       %done
    read_sq(ChosenOption),
    executeChosenMethod(ChosenOption) -> true;
                                        (write('Please choose an option from 1-9 or x. \n'), processQuestions(y)).

%gets tasks greater than a number of hours
executeChosenMethod('4') :-
    write('You have chosen: Which tasks require at least X hours? \n'), flush_output(current_output),
    write('Please provide the minimum length of your desired tasks. \n'), flush_output(current_output),
    write('Number of hours: '), flush_output(current_output),
    read_term(Hrs), 
    number(Hrs) -> (getTasksDur(Hrs, Tasks),
    makeTDurs(Tasks, Msg), write(Msg),  flush_output(current_output), askForInfo) ;
    (write('Invalid input, please try again.\n'), 
    flush_output(current_output), executeChosenMethod('1')).

% creates message given list of durations
makeTDurs([],'').
makeTDurs([duration(TN, D)|Durs], NewRes) :-
    makeTDurs(Durs, Res),
    concatAtomList(['Task Name: ', TN, ', Duration: ', D, '\n', Res], NewRes).

% gets all tasks with duration greater than Hrs
getTasksDur(Hrs, DTasks) :-
    findall(duration(TName, Dur), duration(TName, Dur), Tasks),
    filterTDurs(Hrs, Tasks, DTasks).

%filters tasks less than duration out of the list
filterTDurs(_,[],[]).
filterTDurs(Hrs, [duration(_, D)|Tasks], Res) :-
    \+ compareD(D, Hrs),
    filterTDurs(Hrs, Tasks, Res).
filterTDurs(Hrs, [duration(H, D)|Tasks], [duration(H, D)|Res]) :-
    compareD(D, Hrs),
    filterTDurs(Hrs, Tasks, Res).

% true of Dur is >= Hrs
compareD(Dur, Hrs) :-
    term_to_atom(TD, Dur),
    TD >= Hrs.

% prints task inforation about task name provided by user
executeChosenMethod('1') :-
    write('Task Name: '), flush_output(current_output),
    read_sq(TName),
    makeTaskInfo(TName, Msg) ->
    (write(Msg),  flush_output(current_output),
     askForInfo);
    (write('Task does not exist please try again.\n'), 
    flush_output(current_output), executeChosenMethod('1')).

% makes message of task information of the given task
makeTaskInfo(TName, Msg) :-
    task(TName), due(TName, Date, Time), 
    duration(TName, Dur), prequisite(TName, Pre),
    convertDate(SQDD, Date), timeConvert(Time, CT),
    concatAtomList(['Task Name: ', TName, '\nDue Date: ', SQDD, ', ', CT, '\nDuration : ', Dur, ' hours\n', 'Prerequisite: ', Pre, '\n'], Msg).

% prints event information of the event name provided by user
executeChosenMethod('2') :-
		write('Event Name: '), flush_output(current_output),
		read_sq(EName),
		makeEventInfo(EName, Msg) ->
		(write(Msg), flush_output(current_output),
		askForInfo);
		(write('Event does not exist please try again.\n'), 
    flush_output(current_output), executeChosenMethod('2')).

% makes message for given eventname
makeEventInfo(EName, Msg) :-
		event(EName, Date, range(S, E)),
		convertDate(CD, Date), timeConvert(S, St), timeConvert(E, End),
		concatAtomList(['Event Name: ', EName, '\nDate: ', CD, '\nTime: ', St, ' - ', End, '\n'], Msg).


% prints all availability on a given date based on user input
executeChosenMethod('3') :-
    write('Date (MM/DD/YYYY): '), flush_output(current_output),
    read_sq(ChosenDate),
    convertDate(ChosenDate, CD) ->
    (findall(R, available(CD, R), Ranges), makeRangesInfo(Ranges, Msg),
    write(Msg),  flush_output(current_output), askForInfo);
    (write('Invalid input, please try again.\n'), 
    flush_output(current_output), executeChosenMethod('3')).

% makes a message from a list of ranges
makeRangesInfo([], '').
makeRangesInfo([range(S, E)|Ranges], NewRes) :-
    makeRangesInfo(Ranges, Res),
    timeConvert(S, St), timeConvert(E, End),
    concatAtomList([St, ' - ', End, '\n', Res], NewRes).

% prints a list of all tasks due on a specific day based on user input
executeChosenMethod('5') :-
    write('You have chosen: What task(s) are due on day X? \n'), flush_output(current_output),
    write('Please give a day input of the form MM/DD/YYYY \n'), flush_output(current_output),
    read_sq(GivenDate),
    convertDate(GivenDate, ConvertedDate) -> (findTasksDueOnDay(ConvertedDate, ResultTasks), 
                                            formatTasksDueOnDay(ResultTasks, FormattedResults),
                                            write(FormattedResults), flush_output(current_output),
                                            askForInfo);
                                            (write('Incorrect format given.\n'), executeChosenMethod('5')).

% finds tasks on a day
findTasksDueOnDay(ConvertedDate, ResultTasks) :-
    findall(due(Name, ConvertedDate, _), due(Name, ConvertedDate, _), ResultTasks),
    write('Tasks due on given day: \n'), flush_output(current_output).

%creates message for tasks on a day
formatTasksDueOnDay([],'').
formatTasksDueOnDay([due(TN, D, _)|Durs], NewRes) :-
    formatTasksDueOnDay(Durs, Res), convertDate(CD, D),
    concatAtomList(['Task Name: ', TN, ', DueDate: ', CD, '\n', Res], NewRes).

% prints all dates that have availability on time given by user
executeChosenMethod('6') :-
    write('You have chosen: What days am I available at time X? \n'), flush_output(current_output),
		write('Please Give a time input of form HH:MM (24hr)\n'), flush_output(current_output),
		write('Time: '), flush_output(current_output),
		read_sq(Time), 
		convertTime(Time, CT) -> 
		(findall(available(D, R), available(D, R), Dates), makeDateInfo(CT, Dates, FD),
		formatDateMsg(FD, Msg), write(Msg), flush_output(current_output), askForInfo);
		(write('Invalid input, please try again.\n'), 
        flush_output(current_output), executeChosenMethod('6')).

%creates a message based on list of dates
formatDateMsg([], '').
formatDateMsg([Date|Dates], NewRes) :-
		formatDateMsg(Dates, Res),
		concatAtomList([Date, '\n', Res], NewRes).

% creates list of dates with availability on time CT
makeDateInfo(_, [], []).
makeDateInfo(CT, [available(Date, R)|Dates], [CD|FDates]) :-
		betweenTime(R, CT), convertDate(CD, Date),
		makeDateInfo(CT, Dates, FDates).
makeDateInfo(CT, [available(_, R)|Dates], FDates) :-
		\+ betweenTime(R, CT),
		makeDateInfo(CT, Dates, FDates).

% prints all tasks that have a prerequisite
executeChosenMethod('7') :-
    write('You have chosen: Which tasks have a prerequisite? \n'), flush_output(current_output),
    findall(prequisite(Name, Prereq), (prequisite(Name, Prereq), Prereq \= ''), TasksWithPrereq),
    formatTasksWithPrereq(TasksWithPrereq, ResultTasks),
    write('Tasks with a prerequisite: \n'), flush_output(current_output),
    write(ResultTasks), write('\n'), flush_output(current_output),
    askForInfo.

% makes a message for all task swith a prereq
formatTasksWithPrereq([],'').
formatTasksWithPrereq([prequisite(Name, Prereq)|Prereqs], NewRes) :-
    formatTasksWithPrereq(Prereqs, Res),
    concatAtomList(['Task Name: ', Name, ', Prerequisite: ', Prereq, '\n', Res], NewRes).

% prints out the shortest task
executeChosenMethod('8') :-
    write('You have chosen: What is my shortest task? \n'), flush_output(current_output),
    findall(duration(Name, TaskLength), duration(Name, TaskLength), [D1|Rest]),
    findShortestTask(Rest, D1),
    askForInfo.

% finds the shortest task
findShortestTask([], duration(Name, Length)) :-
    write('Your shortest task is: '), flush_output(current_output),
    write(Name), flush_output(current_output),
    write('\nWhich will take you '), flush_output(current_output),
    write(Length), flush_output(current_output),
    write(' hour(s).\n'), flush_output(current_output).

findShortestTask([duration(Name, TaskLength)|Rest], duration(Name1, TaskLength1)) :-
    TaskLength1 < TaskLength -> findShortestTask(Rest, duration(Name1, TaskLength1));
                                findShortestTask(Rest, duration(Name, TaskLength)).

% prints the longest Task
executeChosenMethod('9') :-
    write('You have chosen: What is my longest task? \n'), flush_output(current_output),
    findall(duration(Name, TaskLength), duration(Name, TaskLength), [D1|Rest]),
    findLongestTask(Rest, D1),
    askForInfo.

% finds the longest task
findLongestTask([duration(Name, TaskLength)|Rest], duration(Name1, TaskLength1)) :-
    TaskLength1 > TaskLength -> findLongestTask(Rest, duration(Name1, TaskLength1));
                                findLongestTask(Rest, duration(Name, TaskLength)).

findLongestTask([], duration(Name, Length)) :-
    write('Your longest task is: '), flush_output(current_output),
    write(Name), flush_output(current_output),
    write('\nWhich will take you '), flush_output(current_output),
    write(Length), flush_output(current_output),
    write(' hour(s).\n'), flush_output(current_output).

%writes out all items in the list followed by a newline
writeResultList([]).
writeResultList([H|T]) :-
    write(H),
    write('\n'),
    writeResultList(T).