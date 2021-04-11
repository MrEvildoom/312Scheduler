:- include('Schedule.pl').

% packages the knowledge base to feed to schedule_list and finds the scheduled list
schedule_list_wrapper(ScheduledList) :- 
    findall(X, task(X), UnscheduledTasks),
    findall(available(X,Y), available(X,Y), AvailableTimes),
    sortTimes(AvailableTimes, SortedAvailableTimes),
    schedule_list(SortedAvailableTimes, UnscheduledTasks, ScheduledList).

% TODO: need a case for when a task has a prerequisite, but that prerequisite has already been scheduled
% right now, we ignore prerequisites
schedule_list(_, [], []).
schedule_list([H_avail|T_avail], [H_task|T_task], [H_scheduled|T_scheduled]) :-
    % prequisite(H_task, ''), % If no prerequisites, schedule in first available time
    % task_fits(H_task, H_avail),
    schedule_task(H_avail, H_task, H_scheduled),
    schedule_list(T_avail, T_task, T_scheduled).

schedule_task(available(Date, Range), Task, scheduledTask(Task, Date, Range)).

% schedule_list(AvailableTimes, [UnschH|UnschT], SchList) :-
%     % If the task has a prerequisite, find that prerequisite and pull it to the front of the list
%     prequisite(UnschH, HPrereq),
%     HPrereq \= '',
%     remove_from_list(HPrereq, UnschT, FixedList),
%     schedule_list(AvailableTimes, [HPrereq, UnschH|FixedList], SchList).

% Helper Relations

% Insertion Sort taken from http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
% and lightly modified to work with times
sortTimes(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).

insert(X,[Y|T],[Y|NT]):-available_after(X, Y), insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-available_leq(X,Y).
insert(X,[],[X]).

% available_before is true if the first argument starts after the second argument.
available_after(available(Date1,_), available(Date2,_)) :-
    beforeDate(Date2, Date1).
available_after(available(Date, range(Start1,_)), available(Date, range(Start2,_))) :-
    beforeTime(Start2, Start1).

% available_leq is true if the first argument starts at the same time as or before the second argument
available_leq(availabe(X, range(Y,_)),available(X, range(Y,_))). % equal
available_leq(X, Y) :- available_after(Y, X). % greater

task_fits(Task, available(_, Range)) :-
    duration(Task, Task_hrs),
    Task_min is (Task_hrs * 60),
    lengthT(Range, Available_min),
    Task_min=<Available_min.

% removes the first instance of item from list
remove_from_list(_, [], []).
remove_from_list(Item, [Item|T], T).
remove_from_list(Item, [H|T], [H|Result]) :-
    remove_from_list(Item, T, Result).

