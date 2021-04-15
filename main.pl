:- include('writer.pl').

mainf :-
    write('Please make sure you have uploaded a valid profile, tasks file, and busy times file! \n Press Enter when ready.\n'), flush_output(current_output),
    read_sq(Ready1),
    catch(load, Err, recoverLoad),
    askForInfo,
    write('Creating a schedule for you...\n'), flush_output(current_output),
    assertSchedule,
    write('Schedule created, writing schedule to CSV...\n'), flush_output(current_output),
    writeToCSV,
    write('Schedule written to output.csv!\n'), flush_output(current_output).

recoverLoad :-
    write('One of the given profile, tasks file, or busy times file is invalid\n'), flush_output(current_output),
    write('Please upload a valid file for each 3! Press Enter when you are ready.\n'), flush_output(current_output),
    read_sq(_),
    catch(load,_,  recoverLoad).

askForInfo :-
    write('Would you like to know some more info about your profile? (y/n) \n'), flush_output(current_output),
    read_sq(YesOrNo),
    checkYes(YesOrNo) -> processQuestions(YesOrNo); 
                    (write('Invalid response. Please answer either yes (y) or no (n) '), askForInfo).

checkYes(y).
checkYes(n).

processQuestions(n) :-
    write('Understood. Moving forward to create a schedule.\n'), flush_output(current_output).
processQuestions(y) :-
    write('What would you like to know?\n'), flush_output(current_output),
    write('(1) Which tasks take at least X hours? \n'), flush_output(current_output), %done B
    write('(2) Show me info for task X \n'), flush_output(current_output), %done B
    write('(3) When am I available on X date? \n'), flush_output(current_output),
    write('(4) Which tasks require X hours or more? \n'), flush_output(current_output), %done
    write('(5) What task(s) are due on day X \n'), flush_output(current_output),        %done
    write('(6) What days am I available at time X? \n'), flush_output(current_output),
    write('(7) Which tasks have a prerequisite? \n'), flush_output(current_output),
    write('(8) What is my shortest task? \n'), flush_output(current_output),            %done
    write('(9) What is my longest task? \n'), flush_output(current_output),       %done
    read_sq(ChosenOption),
    executeChosenMethod(ChosenOption) -> true;
                                        (write('Please choose an option from 1-9 or x. \n'), processQuestions(y)).

%Can't figure out 1-3
executeChosenMethod('1') :-
    write('Number of hours: '), flush_output(current_output),
    read_term(Hrs), getTasksDur(Hrs, Tasks), makeTDurs(Tasks, Msg),
    write(Msg),  flush_output(current_output),
    askForInfo.

makeTDurs([],'').
makeTDurs([duration(TN, D)|Durs], NewRes) :-
    makeTDurs(Durs, Res),
    concatAtomList(['Task Name: ', TN, ', Duration: ', D, '\n', Res], NewRes).

getTasksDur(Hrs, DTasks) :-
    findall(duration(TName, Dur), duration(TName, Dur), Tasks),
    filterTDurs(Hrs, Tasks, DTasks).

filterTDurs(_,[],[]).
filterTDurs(Hrs, [duration(_, D)|Tasks], Res) :-
    \+ compareD(D, Hrs),
    filterTDurs(Hrs, Tasks, Res).
filterTDurs(Hrs, [duration(H, D)|Tasks], [duration(H, D)|Res]) :-
    compareD(D, Hrs),
    filterTDurs(Hrs, Tasks, Res).

compareD(Dur, Hrs) :-
    term_to_atom(TD, Dur),
    TD >= Hrs.

%Can't figure out 1-3
executeChosenMethod('2') :-
    write('Task Name: '), flush_output(current_output),
    read_sq(TName),
    makeTaskInfo(TName, Msg),
    write(Msg),  flush_output(current_output),
    askForInfo.

makeTaskInfo(TName, Msg) :-
    task(TName), due(TName, Date, Time), 
    duration(TName, Dur), prequisite(TName, Pre),
    convertDate(SQDD, Date), timeConvert(Time, CT),
    concatAtomList(['Task Name: ', TName, '\nDue Date: ', SQDD, ', ', CT, '\nDuration : ', Dur, ' hours\n', 'Prerequisite: ', Pre, '\n'], Msg).

%Can't figure out 1-3
executeChosenMethod('3') :-
    write('You have chosen: When am I available on X date? \n'), flush_output(current_output),
    write('Please provide the date to find periods of availability. \n'), flush_output(current_output),
    read_sq(ChosenDate),
    convertDate(ChosenDate, ConvertedDate) -> testDateConvert(ConvertedDate);
                                            (write('Non-numerical value given. Please give a numerical value.\n'), executeChosenMethod('3')).

executeChosenMethod('4') :-
    write('You have chosen: Which tasks require X hours or more? \n'), flush_output(current_output),
    write('Please provide the minimum length of your desired tasks. \n'), flush_output(current_output),
    read_sq(MinLength),
    atom_number(MinLength, ConvertedMinLength) -> (findTasksOfMinLength(ConvertedMinLength), askForInfo);
                                            (write('Non-numerical value given. Please give a numerical value.\n'), executeChosenMethod('4')).

findTasksOfMinLength(X) :-
    findall((taskName=Name, taskLength(hours)= Hours), (duration(Name, Hours), Hours >= X), ResultTasks),
    write('Tasks with minimum length: \n'), flush_output(current_output),
    writeResultList(ResultTasks).
    
%Not sure how to verify the dates are numbers. can just assume correctness?
executeChosenMethod('5') :-
    write('You have chosen: What task(s) are due on day X? \n'), flush_output(current_output),
    write('Please give a day input of the form MM/DD/YYYY \n'), flush_output(current_output),
    read_sq(GivenDate),
    convertDate(GivenDate, ConvertedDate) -> (findTasksDueOnDay(ConvertedDate), askForInfo);
                                            (write('Incorrect format given.\n'), executeChosenMethod('5')).

findTasksDueOnDay(ConvertedDate) :-
    findall((taskName=Name, dueDate=ConvertedDate, dueAt=DueTime), due(Name, ConvertedDate, DueTime), ResultTasks),
    write('Tasks due on given day: \n'), flush_output(current_output),
    writeResultList(ResultTasks).

%Cant figure this out
executeChosenMethod('6') :-
    write('You have chosen: What days am I available at time X? \n'), flush_output(current_output).

%what is an empty cell symbolized as?
executeChosenMethod('7') :-
    write('You have chosen: Which tasks have a prerequisite? \n'), flush_output(current_output),
    write('Tasks with a prerequisite \n'), flush_output(current_output),
    writeResultList(ResultTasks),
    askForInfo.

executeChosenMethod('8') :-
    write('You have chosen: What is my shortest task? \n'), flush_output(current_output),
    findall(duration(Name, TaskLength), duration(Name, TaskLength), [D1|Rest]),
    findShortestTask(Rest, D1),
    askForInfo.

findShortestTask([], duration(Name, Length)) :-
    write('Your shortest task is: '), flush_output(current_output),
    write(Name), flush_output(current_output),
    write('\nWhich will take you '), flush_output(current_output),
    write(Length), flush_output(current_output),
    write('hour(s).\n'), flush_output(current_output).


findShortestTask([duration(Name, TaskLength)|Rest], duration(Name1, TaskLength1)) :-
    TaskLength1 < TaskLength -> findShortestTask(Rest, duration(Name1, TaskLength1));
                                findShortestTask(Rest, duration(Name, TaskLength)).


executeChosenMethod('9') :-
    write('You have chosen: What is my longest task? \n'), flush_output(current_output),
    findall(duration(Name, TaskLength), duration(Name, TaskLength), [D1|Rest]),
    findLongestTask(Rest, D1),
    askForInfo.

findLongestTask([duration(Name, TaskLength)|Rest], duration(Name1, TaskLength1)) :-
    TaskLength1 > TaskLength -> findLongestTask(Rest, duration(Name1, TaskLength1));
                                findLongestTask(Rest, duration(Name, TaskLength)).

findLongestTask([], duration(Name, Length)) :-
    write('Your longest task is: '), flush_output(current_output),
    write(Name), flush_output(current_output),
    write('\nWhich will take you '), flush_output(current_output),
    write(Length), flush_output(current_output),
    write('hour(s).\n'), flush_output(current_output).


writeResultList([]).
writeResultList([H|T]) :-
    write(H),
    write('\n'),
    writeResultList(T).


%dummy data
testDateConvert(ConvertedDate).

due(test1, date(1,3,1997), am(11, 30)).
due(test2, date(1,3,1996), am(11, 30)).
due(test3, date(1,3,1997), pm(11, 30)).

duration(test, 1).
duration(test, 2).
duration(test, 4).
duration(test, 2).
duration(test, 3).
duration(test, 6).



%findall is super helpful, it finds all the stuff in the knoweldge base that satisfies the given constraints with a list
%Can make custom predicates
  
  %cd desktop/CPSC312/312Scheduler