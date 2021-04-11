:- include('Schedule.pl').
:- dynamic slot/2.

start :- auto_load, createSlotsWrapper.

schedule_list_wrapper(Scheduled_List) :-
    findall(X, task(X), Tasks),
    subdivide_tasks(Tasks, Block_List),
    assigned_slots(Block_List, Scheduled_List).
    % prereq_satisfied_wrapper(Scheduled_List).

% divides tasks into blocks of one hour
subdivide_tasks([],[]).
subdivide_tasks([H|T], Result) :-
    duration(H, D),
    replicate(H, D*4, H_Blocks),
    subdivide_tasks(T, T_Blocks),
    append(H_Blocks, T_Blocks, Result).

replicate(_,0,[]).
replicate(Elem, Iter, [Elem|T]) :-
    Iter \= 0,
    NewIter is Iter-1,
    replicate(Elem, NewIter, T).

assigned_slots([],[]).
assigned_slots([H_task|T_task], [assigned(H_task,slot(D,R))|T_Assigned]) :-
    slot(D,R),
    beforeDue(H_task, D, R),
    assigned_slots(T_task, T_Assigned).
    % non_member(assigned(_,slot(D,R)), T_Assigned).

% before_due is true if Date and End are before Task's due date
beforeDue(Task, Date, range(_,Time)) :-
    due(Task, Due_date, Due_time),
    (Date = Due_date -> beforeTime(Time, Due_time) ; beforeDate(Date, Due_date)).

% checks that prerequisites are satisfied
prereq_satisfied_wrapper(X) :- prereq_satisfied(X, X).

prereq_satisfied([],_).
prereq_satisfied([assigned(H,_)|T],List) :-
    prequisite(H,''), prereq_satisfied(T,List).
prereq_satisfied([assigned(H,slot(D,range(S,_)))|T],List) :- 
    prequisite(H, H_prereq),
    H_prereq \= '',
    prereq_iter(D,S,H_prereq, List),
    prereq_satisfied(T,List).

% prereq_iter(Date, Start, Prereq, List) is true if all prereqs in list end before Date, Start. 
prereq_iter(D1, Start, Prereq, [assigned(Prereq,slot(D2, range(_,End))) | T]) :-
    % if Head of the list is a prereq
    (D1 = D2 -> beforeTime(End, Start) ; beforeDate(D2, D1)),
    prereq_iter(D1, Start, Prereq, T).
prereq_iter(D1, Start, Prereq, [assigned(H,_)|T]) :-
    Prereq \= H,
    prereq_iter(D1, Start, Prereq, T).

% Helper functions

before_range(Date1,_,Date2,_) :- beforeDate(Date1, Date2).
before_range(Date, range(_,End1), Date, range(Start2,_)) :- beforeTime(End1,Start2).

non_member(_,[]).
non_member(Elem, [H|T]) :-
    Elem \= H,
    non_member(Elem, T).

createSlotsWrapper :-
    retractall(slot(_,_)),
    findall(available(X, Y), available(X, Y), Availables),
    createSlots(Availables, Slots),
    maplist(assert, Slots).

createSlots([],[]).
createSlots([available(Date, Range)|Availables], AllSlots) :-
  splitTime(Date, Range, SlotsA),
    createSlots(Availables, Slots),
    append(SlotsA, Slots, AllSlots).

splitTime(_, range(End, End), []).
%splitTime(_, range(Start, End), []) :- beforeTime(End, Start).
splitTime(Date, range(Start, End), [slot(Date, range(Start, E15))|Slots]) :-
    timeAfter15(Start, E15),
    splitTime(Date, range(E15, End), Slots).