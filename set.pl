:- module(set, [set_insert/3, member/2, not_member/2, neighbors/3]).
:- include('slot.pl').

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

% member(Elem, Set) is true when member is an element of Set.
member(Elem, set(Elem,_,_)).
member(Elem, set(Node, LT,_)) :-
    before_slot(Elem, Node),
    member(Elem, LT).
member(Elem, set(Node,_, RT)) :-
    before_slot(Node, Elem),
    member(Elem, RT).

% not_member(Elem, set) is true if Elem is not in the set
not_member(_,empty).
not_member(Elem, set(Node, LT,_)) :-
    before_slot(Elem, Node),
    not_member(Elem, LT).
not_member(Elem, set(Node,_,RT)) :-
    before_slot(Node, Elem),
    not_member(Elem, RT).

% calculates how many elements in the set are next to Elem or next to an element next to Elem.
neighbors(Elem, Set, Num) :-
    l_neighbors(Elem, Set, L),
    r_neighbors(Elem, Set, R),
    Num is L + R.

l_neighbors(Elem, Set, Num) :-
    next(N_Elem, Elem),
   (member(N_Elem, Set) ->
       (l_neighbors(N_Elem, Set, N_Num),
        Num is 1 + N_Num) ;
        Num is 0).
l_neighbors(Elem,_, 0) :-
    \+ next(_,Elem).

r_neighbors(Elem, Set, Num) :-
    next(Elem, N_Elem),
   (member(N_Elem, Set) ->
       (r_neighbors(N_Elem, Set, N_Num),
        Num is 1 + N_Num) ;
        Num is 0).
r_neighbors(Elem,_, 0) :-
    \+ next(Elem,_).

% next(slot(A,B), slot(C,D)) :- slot_next(slot(A,B), slot(C,D)).
% next(A,B) :- number(A), not(var(A)), var(B), B is A + 1.
% next(A,B) :- number(B), var(A), not(var(B)), A is B - 1.