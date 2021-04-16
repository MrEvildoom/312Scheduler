%% File for all slots operations %%
:- dynamic slot/2.

createSlotsWrapper :-
    retractall(slot(_,_)),
    findall(available(X, Y), available(X, Y), Availables),
    createSlots(Availables, Slots),
    filterEvents(Slots, FSlots),
    maplist(assert, FSlots).

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

%filters out slots that are during events
filterEvents([],[]).
filterEvents([slot(Date, range(S, E))|Slots], [slot(Date, range(S, E))|FSlots]) :-
		\+ duringEvent(Date, S,_),
		filterEvents(Slots, FSlots).
filterEvents([slot(Date, range(S,_))|Slots], FSlots) :-
		duringEvent(Date, S,_),
        %assert(assigned(Name, slot(Date, range(S, E)))),
		filterEvents(Slots, FSlots).

duringEvent(Date, Time, Name) :-
		event(Name, Date, Range),
		betweenTimeNE(Range, Time).

% before_slot(Slot1, Slot2) is true if slot1 ends before or when slot2 starts
before_slot(slot(D1,_), slot(D2,_)) :-
    beforeDate(D1, D2).
before_slot(slot(D, range(_,Time)), slot(D, range(Time,_))).
before_slot(slot(D, range(_,End)), slot(D, range(Start,_))) :-
    beforeTime(End, Start).

% slotRightAfter(A, B) is true if B is right after A
slotRightAfter(slot(D, range(A, B)), slot(D, range(B, C))) :-
    slot(D, range(A,B)),
    slot(D, range(B,C)).
