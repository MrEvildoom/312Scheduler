%% File for all date time operations %%

% takes in a single quote date item and returns the datre in format date(MM, DD, YYYY)
convertDate(Date, date(MM, DD, YYYY)) :-
    term_to_atom(MM/DD/YYYY, Date).

% converts a single quote time item into am(HH, MM) or pm(HH, MM) only work going from 'HH:MM' to am(HH, MM)
convertTime(Time, am(HH, MM)) :-
    term_to_atom(HH:MM, Time),
    HH < 12, HH > 0.
convertTime(Time, am(12, MM)) :-
    term_to_atom(0:MM, Time).
convertTime(Time, pm(NH, MM)) :-
    term_to_atom(HH:MM, Time),
    NH is HH - 12, HH > 12.
convertTime(Time, pm(12, MM)) :-
    term_to_atom(12:MM, Time).

% converts an am/pm time into a single quote time.  only work going from 'HH:MM' to am(HH, MM)
timeConvert(am(H, M), Time) :-
    term_to_atom(H:M, Time),
    H < 12.
timeConvert(am(12, M), Time) :-
    term_to_atom(0:M, Time).
timeConvert(pm(H, M), Time) :-
    H < 12, PH is H + 12,
    term_to_atom(PH:M, Time).
timeConvert(pm(12, M), Time) :-
    term_to_atom(12:M, Time).

% timeConvert(am(H, M), Time) :-
%     term_to_atom(H:NM, Time),
%     H < 12, M < 10, atom_concat(0,M,NM).
% timeConvert(am(12, M), Time) :-
%     term_to_atom(0:NM, Time),
%     M < 10, atom_concat(0,M,NM).
% timeConvert(pm(H, M), Time) :-
%     atom_concat(0,M,NM),
%     term_to_atom(PH:NM, Time),
%     H < 12, PH is H + 12, M < 10.
% timeConvert(pm(12, M), Time) :-
%     term_to_atom(12:NM, Time),
%     M < 10, atom_concat(0,M,NM).

% beforeTime(T1, T2) is true if time 1 (is a time) and is less than time 2 (which is also a time)
beforeTime(am(_,_), pm(_,_)).

beforeTime(am(12,_), am(H2,_)) :- H2 < 12.
beforeTime(am(H1,_), am(H2,_)) :- (H1<H2), (H2 < 12).
beforeTime(am(H1,M1), am(H1,M2)) :- (M1<M2).

beforeTime(pm(12,_), pm(H2,_)) :- H2 < 12.
beforeTime(pm(H1,_), pm(H2,_)) :- (H1<H2), (H2 < 12).
beforeTime(pm(H1,M1), pm(H1,M2)) :- (M1<M2).

% beforeDate(D1, D2) is true when date D1 is before date D2
beforeDate(date(_,_,Y0), date(_,_,Y1)) :-
    Y0 < Y1.
beforeDate(date(_,M0,Y), date(_,M1,Y)) :-
    M0 < M1.
beforeDate(date(D0,M,Y), date(D1,M,Y)) :-
    D0 < D1.

% lengthT(range(Start, End), Length) is true if L is the length in minutes of time between start and end
lengthT(range(am(H,M1), am(H, M2)), L) :- L is M2 - M1.
lengthT(range(pm(H,M1), pm(H, M2)), L) :- L is M2 - M1.
lengthT(range(am(H1, M1), pm(H2, M2)), L) :-
    H1 < 12, H2 < 12,
    L is ((12-H1) + H2)*60 + M2 - M1.
lengthT(range(am(12, M1), pm(H2, M2)), L) :-
    H2 < 12,
    L is (12 + H2)*60 + M2 - M1.
lengthT(range(am(H1, M1), pm(12, M2)), L) :-
    H1 < 12,
    L is (12 - H1)*60 + M2 - M1.
lengthT(range(am(12, M1), pm(12, M2)), L) :-
    L is 12*60 + M2 - M1.

lengthTHours(Range, L) :-
    lengthT(Range, LM),
    L is LM / 60.

% returns the time of the given time 15 minutes after.
timeAfter15(T1, T2) :-
    timeAfterX(T1, T2, 15).
% timeAfter15(am(H, M1), am(H, M2)) :-
% 		M1 < 45,
% 		M2 is M1 + 15.
% timeAfter15(pm(H, M1), pm(H, M2)) :-
% 		M1 < 45,
% 		M2 is M1 + 15.
% timeAfter15(am(H1, M1), am(H2, M2)) :-
% 		H1 < 11, M1 > 44,
% 		H2 is H1 + 1,
% 		M2 is M1 - 45.
% timeAfter15(pm(H1, M1), pm(H2, M2)) :-
% 		H1 < 11, M1 > 44,
% 		H2 is H1 + 1,
% 		M2 is M1 - 45.
% timeAfter15(am(12, M1), am(1, M2)) :-
% 		M1 > 44,
% 		M2 is M1 - 45.
% timeAfter15(pm(12, M1), pm(1, M2)) :-
% 		M1 > 44,
% 		M2 is M1 - 45.
% timeAfter15(am(11, M1), pm(12, M2)) :-
% 		M1 > 44,
% 		M2 is M1 - 45.

% returns the time of the given time 30 minutes after.
timeAfter30(T1, T2) :-
    timeAfterX(T1, T2, 30).
% timeAfter30(am(H, M1), am(H, M2)) :-
% 		M1 < 30,
% 		M2 is M1 + 30.
% timeAfter30(pm(H, M1), pm(H, M2)) :-
% 		M1 < 30,
% 		M2 is M1 + 30.
% timeAfter30(am(H1, M1), am(H2, M2)) :-
% 		H1 < 11, M1 > 29,
% 		H2 is H1 + 1,
% 		M2 is M1 - 30.
% timeAfter30(pm(H1, M1), pm(H2, M2)) :-
% 		H1 < 11, M1 > 29,
% 		H2 is H1 + 1,
% 		M2 is M1 - 30.
% timeAfter30(am(12, M1), am(1, M2)) :-
% 		M1 > 29,
% 		M2 is M1 - 30.
% timeAfter30(pm(12, M1), pm(1, M2)) :-
% 		M1 > 29,
% 		M2 is M1 - 30.
% timeAfter30(am(11, M1), pm(12, M2)) :-
% 		M1 > 29,
% 		M2 is M1 - 30.

% returns the time of the given time X minutes after.
timeAfterX(am(H, M1), am(H, M2), X) :-
		M1 < X,
		M2 is M1 + X.
timeAfterX(pm(H, M1), pm(H, M2), X) :-
		M1 < X,
		M2 is M1 + X.
timeAfterX(am(H1, M1), am(H2, M2), X) :-
		H1 < 11, M1 > X-1,
		H2 is H1 + 1,
		M2 is M1 - X.
timeAfterX(pm(H1, M1), pm(H2, M2), X) :-
		H1 < 11, M1 > X-1,
		H2 is H1 + 1,
		M2 is M1 - X.
timeAfterX(am(12, M1), am(1, M2), X) :-
		M1 > X-1,
		M2 is M1 - X.
timeAfterX(pm(12, M1), pm(1, M2), X) :-
		M1 > X-1,
		M2 is M1 - X.
timeAfterX(am(11, M1), pm(12, M2), X) :-
		M1 > X-1,
		M2 is M1 - X.

% gives the next day of the month
nextDay(date(M1,D1,Y), date(M2,1,Y)) :-
	  monthend(M1, D1),
    M2 is M1 + 1.
nextDay(date(M,D1,Y), date(M,D2,Y)) :-
		\+ monthend(M,D1),
		D2 is D1 + 1.
nextDay(date(12,31,Y1), date(1,1,Y2)) :-
		Y2 is Y1 + 1.

% monthend(M, D) is true if day d is the last day of month M
monthend(1, 31).
monthend(2, 28).
monthend(3, 31).
monthend(4, 30).
monthend(5, 31).
monthend(6, 30).
monthend(7, 31).
monthend(8, 31).
monthend(9, 30).
monthend(10, 31).
monthend(11, 30).
% monthend(12, 31).

% rounds time down to a 30 min interval unless already on one
roundDown30(am(H, M), am(H,NM)) :-
    M < 30, NM is 0.
roundDown30(am(H, M), am(H,NM)) :-
    M > 30, NM is 30.
roundDown30(pm(H, M), pm(H,NM)) :-
    M < 30, NM is 0.
roundDown30(pm(H, M), pm(H,NM)) :-
    M > 30, NM is 30.
roundDown30(am(H,30), am(H, 30)).
roundDown30(pm(H,30), pm(H, 30)).

% rounds time up to a 30 min interval unless already on one
roundUp30(am(H, M), am(H,NM)) :-
    M < 31, M >0, NM is 30.
roundUp30(am(H, M), T) :-
    M > 30,
    timeAfter30(am(H,30), T).
roundUp30(pm(H, M), pm(H,NM)) :-
    M < 31, M > 0, NM is 30.
roundUp30(pm(H, M), T) :-
    M > 30,
    timeAfter30(pm(H,30), T).
roundUp30(am(H, 0), am(H, 0)).
roundUp30(pm(H, 0), pm(H, 0)).