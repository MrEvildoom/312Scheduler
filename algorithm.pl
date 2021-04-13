:- include('data.pl').
:- dynamic slot/2.

start :- auto_load.

schedule_list(Scheduled_List) :-
    createSlotsWrapper,
    findall(X, task(X), Tasks),
    % prereq_sort(Tasks, Sorted_Tasks),
    % subdivide_tasks(Sorted_Tasks, Block_List),
    subdivide_tasks(Tasks, Block_List),
    assigned_slots_wrapper(Block_List, Scheduled_List),
    prereq_satisfied_wrapper(Scheduled_List).

prereq_sort(List, Sorted_List) :-
    prioritize_tasks(List, Priority_List),
    insert_sort(Priority_List, Sort_P_List),
    strip_priority(Sort_P_List, Sorted_List).

% findall(X, task(X), Tasks), order_tasks(Tasks, Ordered), insert_sort(Ordered, X).
strip_priority([],[]).
strip_priority([prioritized(X,_)|T], [X|T_res]) :-
    strip_priority(T, T_res).

prioritize_tasks([],[]).
prioritize_tasks([H|T], [prioritized(H,P)|T_ordered]) :-
    prereq_priority(H,P),
    prioritize_tasks(T, T_ordered).

% prereq_priority(Task, Priority) is true when Priority is the length of prereq-chain behind Task
prereq_priority(Task, 0) :- prequisite(Task,'').
prereq_priority(Task, Priority) :-
    prequisite(Task, Prereq),
    prereq_priority(Prereq, P_Priority),
    Priority is P_Priority + 1.

% from http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
% from http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
% altered to work with prereq priority
insert(prioritized(_,X),[prioritized(_,Y)|T],[prioritized(_,Y)|NT]):-X>Y,insert(X,T,NT).
insert(prioritized(_,X),[prioritized(_,Y)|T],[prioritized(_,X),prioritized(_,Y)|T]):-X=<Y.
insert(X,[],[X]).

% divides tasks into blocks of one hour
subdivide_tasks([],[]).
subdivide_tasks([H|T], Result) :-
    duration(H, D),
    replicate(H, D*2, H_Blocks),        
    subdivide_tasks(T, T_Blocks),
    append(H_Blocks, T_Blocks, Result).

replicate(_,0,[]).
replicate(Elem, Iter, [Elem|T]) :-
    Iter \= 0,
    NewIter is Iter-1,
    replicate(Elem, NewIter, T).

assigned_slots_wrapper(Tasks, Tasklist) :-
    assigned_slots(Tasks, Tasklist, _).

assigned_slots([],[], empty).
assigned_slots([H_task|T_task], [assigned(H_task,slot(D,R))|T_Assigned], New_Set) :-
    assigned_slots(T_task, T_Assigned, Set),
    is_valid_slot(slot(D,R)),
    not_member(slot(D,R), Set),
    set_insert(slot(D,R), Set, New_Set).
    % beforeDue(H_task, D, R), 
    % non_member(assigned(_,slot(D,R)), T_Assigned).

% is_valid_slot(Slot) is true when Slot is in the Knowledge Base
is_valid_slot(slot(D,Range)) :- slot(D,Range).

% set(Node, LT, RT) where LT and RT are either another set or empty.
% set_insert(Elem, Set, Newset) is true when Newset is the union of Set and {Elem}
set_insert(Elem, empty, set(Elem, empty, empty)).
set_insert(Elem, set(Elem, LT, RT), set(Elem, LT, RT)).
set_insert(Elem, set(S_Elem, LT, RT), set(S_Elem, NLT, RT)) :-
    before_slot(Elem, S_Elem),
    set_insert(Elem, LT, NLT).
set_insert(Elem, set(S_Elem, LT, RT), set(S_Elem, LT, NRT)) :-
    before_slot(S_Elem, Elem),
    set_insert(Elem, RT, NRT).

member(Elem, set(Elem,_,_)).
member(Elem, set(Node, LT,_)) :-
    before_slot(Elem, Node),
    member(Elem, LT).
member(Elem, set(Node,_, RT)) :-
    before_slot(Node, Elem),
    member(Elem, RT).

% not_member(Elem, set) is true if Elem is not in the set
not_member(_, empty).
not_member(Elem, set(Node, LT,_)) :-
    before_slot(Elem, Node),
    not_member(Elem, LT).
not_member(Elem, set(Node,_,RT)) :-
    before_slot(Node, Elem),
    not_member(Elem, RT).

% before_due is true if Date and End are before Task's due date
before_due(Task, Date, range(_,Time)) :-
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

% Helper functions

% before_slot(Slot1, Slot2) is true if slot1 ends before or when slot2 starts
before_slot(slot(D1,_), slot(D2,_)) :-
    beforeDate(D1, D2).
before_slot(slot(D, range(_,Time)), slot(D, range(Time,_))).
before_slot(slot(D, range(_,End)), slot(D, range(Start,_))) :-
    beforeTime(End, Start).


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


