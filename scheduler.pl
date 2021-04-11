:- include('Schedule.pl', 'transformation.pl').
:- dynamic slot/3.



schedule_list_wrapper(ScheduledList) :-
    findall(X, task(X), Tasks),
    subdivide_tasks(Tasks, Block_List),
    assigned_slots(Block_List, Scheduled_List).
    % prerequisites_satisfied(Block_List, ScheduledList).

% divides tasks into blocks of one hour
subdivide_tasks([],[]).
subdivide_tasks([H|T], Result) :-
    duration(H, D),
    replicate(H, D, H_Blocks),
    subdivide_tasks(T, T_Blocks),
    append(H_Blocks, T_Blocks, Result).

replicate(_,0,[]).
replicate(Elem, Iter, [Elem|T]) :- replicate(Elem, Iter-1, T).

assigned_slots([],[]).
assigned_slots([H|T], [assigned(H,slot(D,S,E))|T_Assigned]) :-
    slot(D,range(S,E)),
    assigned_slots(T, T_Assigned).


scheduled_list([], _).
scheduled_list([H_task|T_task], Scheduled_tasks_list) :-
    member(scheduledTask(Task, available(_,range(_,_))), Scheduled_tasks_list),
    scheduled_list(T_task, Scheduled_tasks_list).

% checks that prerequisites are satisfied
prerequisites_satisfied([],_).
prerequisites_satisfied([H_task|T_task], Scheduled_tasks_list) :-
    prequisite(H_task, ''),
    prerequisites_satisfied(T_task, Scheduled_tasks_list).
prerequisites_satisfied([H_task|T_task], Scheduled_tasks_list) :-
    prequisite(H_task, H_t_p),
    member(scheduledTask(H_task, available(H_task_date,H_task_time)), Scheduled_tasks_list),
    member(scheduledTask(H_t_p, available(H_t_p_date,H_t_p_time)), Scheduled_tasks_list),
    before_range(H_task_date, H_task_time, H_t_p_date, H_t_p_time),
    prerequisites_satisfied(T_task, Scheduled_tasks_list).

% Helper functions

before_range(Date1,_,Date2,_) :- beforeDate(Date1, Date2).
before_range(Date, range(_,End1), Date, range(Start2,_)) :- beforeTime(End1,Start2).

% a slot is valid if it is during available time and at least one hour
slot(Date, range(Start, End)) :-
    slot_available(slot(Date, range(Start, End))),
    time_length(range(Start, End), 60).

slot_available(slot_available(slot(Date, range(Start, End))) :-
    available(Date, range(A_Start, A_End)),
    beforeTime()