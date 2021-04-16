:- include('data.pl').
:- include('slot.pl').
:- use_module(set).
:- dynamic slot/2, assigned/2, max_time/1.

%create a schedule and assert assigned slots into the knowledge base
assertSchedule :-
        %retractall(assigned(_,_)),
        findall(assigned(X,Y), assigned(X,Y), []),
		schedule_list(SL),
        assertFacts(SL).

%creates a list of scheduled items
schedule_list(Scheduled_List) :-
    once(createSlotsWrapper),
    findall(X, task(X), Tasks),
    enough_time(Tasks),
    subdivide_tasks(Tasks, Block_List),
    assign_slots(Block_List, Scheduled_List, _),
    % insert_sort(Scheduled_List, Ordered_List),
    prereq_satisfied_wrapper(Scheduled_List).

% enough_time(Tasks) is true if there is enough time to do all tasks
% assuming we take the maximum number of breaks
enough_time(Tasks) :-
    max_time(X),
    (X \= 0 -> Task_Time_Adj is 1 / X ; Task_Time_Adj is 0),
    sum_time(Tasks, Task_Time),
    Real_Task_Time is Task_Time + (Task_Time * Task_Time_Adj),
    findall(slot(D,R), slot(D,R), Slots),
    length(Slots, L),
    Time_available is L / 2,
    Real_Task_Time =< Time_available.

sum_time([],0).
sum_time([H|T], N1) :-
    duration(H,D),
    sum_time(T, N2),
    N1 is D + N2.

% divides tasks into blocks of one hour
subdivide_tasks([],[]).
subdivide_tasks([H|T], Result) :-
    duration(H, D),
    replicate(H, D*2, H_Blocks),        
    subdivide_tasks(T, T_Blocks),
    append(H_Blocks, T_Blocks, Result).

% assign_slots(Tasks, Tasklist, Set) is true if for each element Task of Tasks, there is a assigned(Task, slot(_,_)) in Tasklist.
assign_slots([],[], empty).
assign_slots([H_Task|T_task], [assigned(H_Task,slot(D,R))|T_Assigned], New_Set) :-
    assign_slots(T_task, T_Assigned, Set),
    slot(D,R),
    before_due(H_Task, slot(D,R)),
    not_member(slot(D,R), Set),
    break_check(slot(D,R), Set),
    set_insert(slot(D,R), Set, New_Set).

% before_due is true if Date and End are before Task's due date
before_due(Task, slot(Date, range(_,Time))) :-
    due(Task, Due_date, Due_time),
    (Date == Due_date -> beforeTime(Time, Due_time) ; beforeDate(Date, Due_date)).

max_time(0).
% the maximum amount of slots that can scheduled consecutively
% 0 means no limit

% break_check(Elem, Set) is true if inserting Elem into Set
% would not cause Set to include more than the max allowable length on uninterrupted elements.
break_check(_,_) :- max_time(0).
break_check(Slot, Set) :-
    neighbors(Slot, Set, N),
    max_time(Max),
    N1 is N + 1,
    Max >= N1.

% prereq_satisfied_wrapper is true if every assigned(task, slot) with a prereq has no slot with that prereq occuring after it
prereq_satisfied_wrapper(X) :- prereq_satisfied(X, X).
% TODO: now that we know input is ordered, we can make this more efficient
prereq_satisfied([],_).
prereq_satisfied([assigned(H,_)|T],List) :-
    prequisite(H,''), prereq_satisfied(T,List).
prereq_satisfied([assigned(H,Slot)|T] , List) :- 
    prequisite(H, H_prereq),
    H_prereq \= '',
    prereq_iter(Slot,H_prereq, List),
    prereq_satisfied(T,List).

% prereq_iter(Date, Start, Prereq, List) is true if all prereqs in list end before Date, Start. 
prereq_iter(_,_,[]).
prereq_iter(Slot, Prereq, [assigned(Prereq,Pre_Slot) | T]) :-
    % if Head of the list is a prereq
    before_slot(Pre_Slot, Slot),
    prereq_iter(Slot, Prereq, T).
prereq_iter(Slot, Prereq, [assigned(H,_)|T]) :-
    Prereq \= H,
    prereq_iter(Slot, Prereq, T).


% helper functions

% take(List1, N, List2) is true when List2 is the first N elements of List1
take(_,0,[]).
take([H|T1], N, [H|T2]) :- N > 0, N_New is N-1, take(T1, N_New, T2).

% replicate(Elem, Int, List) is true if List is a list of Int Elems.
replicate(_,0,[]).
replicate(Elem, Iter, [Elem|T]) :-
    Iter \= 0,
    NewIter is Iter-1,
    replicate(Elem, NewIter, T).