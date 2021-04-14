:- module(algorithm, [schedule_list/1]).

:- include('data.pl').
:- use_module(set).
:- dynamic slot/2.



schedule_list(Scheduled_List) :-
    createSlotsWrapper,
    findall(X, task(X), Tasks),
    subdivide_tasks(Tasks, Block_List),
    assign_slots_wrapper(Block_List, Scheduled_List),
    prereq_satisfied_wrapper(Scheduled_List).

% divides tasks into blocks of one hour
subdivide_tasks([],[]).
subdivide_tasks([H|T], Result) :-
    duration(H, D),
    replicate(H, D*2, H_Blocks),        
    subdivide_tasks(T, T_Blocks),
    append(H_Blocks, T_Blocks, Result).

% assigned_slots_wrapper(Tasks, Tasklist) is true if Tasklist is a list of assigned(task, slot),
% with a task for every element in Tasks and no repeated slots
assign_slots_wrapper(Tasks, Tasklist) :-
    assign_slots(Tasks, Tasklist, _).

assign_slots([],[], empty).
assign_slots([H_task|T_task], [assigned(H_task,slot(D,R))|T_Assigned], New_Set) :-
    assign_slots(T_task, T_Assigned, Set),
    slot(D,R),
    before_due(H_Task, slot(D,R)),
    not_member(slot(D,R), Set),
    set_insert(slot(D,R), Set, New_Set).

% before_due is true if Date and End are before Task's due date
before_due(Task, slot(Date, range(_,Time))) :-
    due(Task, Due_date, Due_time),
    (Date = Due_date -> beforeTime(Time, Due_time) ; beforeDate(Date, Due_date)).

% prereq_satisfied_wrapper is true if every assigned(task, slot) with a prereq has no slot with that prereq occuring after it
prereq_satisfied_wrapper(X) :- prereq_satisfied(X, X).

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

% not fully working yet, will finish tonight
splitTime(Date, range(S, E), Slots) :-
    roundUp30(S, Start),
    roundDown30(E, End),
    splitTimeH(Date, range(Start,End), Slots).

splitTimeH(Date, range(Start, End), [slot(Date, range(Start, E15))|Slots]) :-
		timeAfterX(Start, E15, 30), beforeTime(E15, End),
		splitTimeH(Date, range(E15, End), Slots).
splitTimeH(Date, range(Start, E15), [slot(Date, range(Start, E15))]) :-
		timeAfterX(Start, E15, 30).

%%% Helper functions %%%

% replicate(Elem, Int, List) is true if List is a list of Int Elems.
replicate(_,0,[]).
replicate(Elem, Iter, [Elem|T]) :-
    Iter \= 0,
    NewIter is Iter-1,
    replicate(Elem, NewIter, T).

% before_slot(Slot1, Slot2) is true if slot1 ends before or when slot2 starts
before_slot(slot(D1,_), slot(D2,_)) :-
    beforeDate(D1, D2).
before_slot(slot(D, range(_,Time)), slot(D, range(Time,_))).
before_slot(slot(D, range(_,End)), slot(D, range(Start,_))) :-
    beforeTime(End, Start).