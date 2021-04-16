:- include('data.pl').
:- include('slot.pl').
:- use_module(set).
:- dynamic slot/2, assigned/2.

assertSchedule :-
		schedule_list(SL),
        assertFacts(SL).

schedule_list(Ordered_List) :-
    createSlotsWrapper,
    findall(X, task(X), Tasks),
    subdivide_tasks(Tasks, Block_List),
    assign_slots(Block_List, Scheduled_List, _),
    insert_sort(Scheduled_List, Ordered_List),
    prereq_satisfied_wrapper(Ordered_List).
    % ensure_breaks(Ordered_List).

% findall(X, task(X), Tasks), assign_slots_wrapper(Tasks, X), insert_sort(X,Y).

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

% break_check(Elem, Set) is true if inserting Elem into Set
% would not cause Set to include more than the max allowable length on uninterrupted elements.
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



insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).

insert(assigned(X,XS),[assigned(Y,YS)|T],[assigned(Y,YS)|NT]) :- 
    before_slot(YS,XS),insert(assigned(X,XS),T,NT).
insert(assigned(X,XS),[assigned(Y,YS)|T],[assigned(X,XS),assigned(Y,YS)|T]) :- 
    before_slot(XS,YS).
insert(X,[],[X]).



max_time(4). % the maximum amount of slots that can scheduled consecutively

% ensures that there are not five assigned time slots in a row
ensure_breaks(List) :- max_time(Max), length(List, Len), Len =< Max.
    % true if there is not enough slots to exceed max
ensure_breaks([H|T]) :- 
    max_time(Max),
    Max1 is Max+1,
    take([H|T], Max1, Front),
    contains_break(Front),
    ensure_breaks(T).

% contains_break(List_Assigned) is true if there is at least one pair of non-consecutive assigned_tasks adjacent in the list
% lists with less than two elements do not contain a pair of non-consequtive elements.
contains_break([assigned(_,slot(DateA,range(_,EndA))),
                assigned(_,slot(DateB,range(StartB,_)))]) :- 
                    DateA \= DateB ; EndA \= StartB. % pretty sure midnight breaks this, let's hope that doesn't come up
contains_break([assigned(_,slot(DateA,range(_,EndA))),
                assigned(B,slot(DateB,range(StartB,EndB)))|T]) :-
                    T \= [], 
                    (DateA \= DateB ; EndA \= StartB) ;
                    contains_break([assigned(B,slot(DateB,range(StartB,EndB)))|T]).

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