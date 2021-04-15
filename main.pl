:- include('writer.pl').

mainf :-
    write('Please make sure you have uploaded a valid profile, tasks file, and busy times file! \n Press Enter when ready.\n'), flush_output(current_output),
    read_sq(Ready1),
    catch(load, Err, recoverLoad),
    askForInfo,
    write('Creating a schedule for you...\n'), flush_output(current_output),
<<<<<<< HEAD
    createSlotsWrapper, % not sure if this is the start of the algorithm
=======
    assertSchedule, %assertSchedule, % not sure if this is the start of the algorithm
>>>>>>> a5135c2fef4c7d2f399c51cdef669be19ab42e72
    write('Schedule created, writing schedule to CSV...\n'), flush_output(current_output),
    writeToCSV, %- wait for jack to create correct assertions for assignments.
    write('Schedule written to output.csv!\n'), flush_output(current_output).

recoverLoad :-
    write('One of the given profile, tasks file, or busy times file is invalid\n'), flush_output(current_output),
    write('Please upload a valid file for each 3! Press Enter when you are ready.\n'), flush_output(current_output),
    read_sq(_),
    catch(load,_,  recoverLoad).

askForInfo :-
    write('Would you like to know some info about your profile? (y/n) \n'), flush_output(current_output),
    read_sq(YesOrNo),
    checkYes(YesOrNo) -> processQuestions(YesOrNo); 
                    (write('Invalid response. Please answer either yes (y) or no (n) '), askForInfo).

checkYes(y).
checkYes(n).

processQuestions(n) :-
    write('Understood. Moving forward to create a schedule.\n'), flush_output(current_output).
processQuestions(y) :-
    write('What would you like to know?\n'), flush_output(current_output),
    write('(1) Which tasks take at least X hours? \n'), flush_output(current_output),
    write('(2) When am I available on X date? \n'), flush_output(current_output),
    read_sq(ChosenOption),
    executeChosenMethod(ChosenOption).

executeChosenMethod('1') :-
    write('You have chosen: Which tasks take at least X hours? \n'), flush_output(current_output),
    write('Please provide the minimum length of your desired tasks. \n'), flush_output(current_output),
    read_sq(MinLength).

executeChosenMethod('2') :-
    write('You have chosen: When am I available on X date? \n'), flush_output(current_output),
    write('Please provide the minimum length of your desired tasks. \n'), flush_output(current_output),
    read_sq(ChosenDate).

%findall is super helpful, it finds all the stuff in the knoweldge base that satisfies the given constraints with a list
%Can make custom predicates
  
  %cd desktop/CPSC312/312Scheduler