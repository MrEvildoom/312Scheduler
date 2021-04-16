%% File for all date time operations %%
:- module(datetime, 
    [convertDate/2, convertTime/2, timeConvert/2,
    convertTimeRange/2,
    beforeTime/2, beforeDate/2,
    lengthT/2,
    timeAfterX/3, timeAfter15/2, timeAfter30/2,
    no_midnight/2,
    nextDay/2,
    roundDown30/2, roundUp30/2,
    betweenTime/2, betweenTimeNE/2]).
:- dynamic slot/2, event/3.

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

% converts an am/pm time into a single quote time.  only work going from am(HH, MM) to 'HH:MM' 
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

% convertTimeRange takes a lsit of time ranges and returns a list formatted as range(am(11,00) ,pm(1,00))
convertTimeRange([],[]).
convertTimeRange([TimeR|Times], [range(StartTime, EndTime)|ListRange]) :-
    term_to_atom(SH:SM-EH:EM, TimeR), term_to_atom(SH:SM, ST), term_to_atom(EH:EM, ET),
    convertTime(ST, StartTime),
    convertTime(ET, EndTime),
    convertTimeRange(Times, ListRange).

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

%lengthTHours is true if L is the length in Hours of the range
lengthTHours(Range, L) :-
    lengthT(Range, LM),
    L is LM / 60.

% returns the time of the given time 15 minutes after.
timeAfter15(T1, T2) :-
    timeAfterX(T1, T2, 15).

% returns the time of the given time 30 minutes after.
timeAfter30(T1, T2) :-
    timeAfterX(T1, T2, 30).

% true if B is the time after X of A
timeAfterX(A,B,X) :-
    nonvar(A), nonvar(B), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXForward(A,B1,X),
    B = B1.

timeAfterX(A,am(B,C),X) :-
    nonvar(A), var(B), var(C), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXForward(A,am(B,C),X).
timeAfterX(A,pm(B,C),X) :-
    nonvar(A), var(B), var(C), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXForward(A,pm(B,C),X).
timeAfterX(A,B,X) :-
    nonvar(A), var(B), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXForward(A,B,X).

timeAfterX(am(A,B),C,X) :-
    var(A), var(B), nonvar(C), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXBackward(am(A,B),C,X).
timeAfterX(pm(A,B),C,X) :-
    var(A), var(B), nonvar(C), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXBackward(pm(A,B),C,X).
timeAfterX(A,B,X) :-
    var(A), nonvar(B), nonvar(X),
    X >= 0, X < 1440,
    timeAfterXBackward(A,B,X).

%% Forward %%

% adding more than 60 minutes
timeAfterXForward(A,C,X) :-
    nonvar(A), var(C), nonvar(X), X>60,
    X1 is X - 60,
    timeAfterXForward(A,B,60),
    timeAfterXForward(B,C,X1).
% adding minutes without going over the hour
timeAfterXForward(am(H, M1), am(H, M2), X) :-
    nonvar(H), nonvar(M1), nonvar(X), X=<60,
    M2 is M1 + X,
    M2 < 60.
% adding minutes going to or over the hour
% 1 am to 10 am
timeAfterXForward(am(H1, M1), am(H2, M2), X) :-
    nonvar(H1), nonvar(M1), nonvar(X), X=<60,
    M3 is M1 + X,
    M3 >= 60,
    M2 is M3 - 60,
    H1 < 11,
    H2 is H1 + 1.
% adding minutes going to or over the hour, 11 am to 12 pm
timeAfterXForward(am(11, M1), pm(12, M2), X) :-
    nonvar(M1), nonvar(X), X=<60,
    M3 is M1 + X,
    M3 >= 60,
    M2 is M3 - 60.
% adding minutes going to or over the hour, 12 am to 1 am
timeAfterXForward(am(12, M1), am(1, M2), X) :-
    nonvar(M1), nonvar(X), X=<60,
    M3 is M1 + X,
    M3 >= 60,
    M2 is M3 - 60.

timeAfterXForward(pm(A,B), am(C,D), X) :-
    nonvar(A), nonvar(B), var(C), var(D), nonvar(X),
    timeAfterXForward(am(A,B), pm(C,D),X).

timeAfterXForward(pm(A,B), pm(C,D), X) :-
    nonvar(A), nonvar(B), var(C), var(D), nonvar(X),
    timeAfterXForward(am(A,B), am(C,D),X).

%% Backward %%

% subtracting more than 60 minutes
timeAfterXBackward(A,C,X) :-
    var(A), nonvar(C), nonvar(X), X>60,
    X1 is X - 60,
    timeAfterXBackward(B,C,60),
    timeAfterXBackward(A,B,X1).
% subtracting minutes without going over the hour
timeAfterXBackward(am(H, M1), am(H, M2), X) :-
    nonvar(H), var(M1), nonvar(M2), nonvar(X), X=<60,
    M1 is M2 - X,
    M1 >= 0.
% subtracting minutes going to or over the hour
% 2 am to 11 am
timeAfterXBackward(am(H1, M1), am(H2, M2), X) :-
    var(H1), var(M1), nonvar(H2), nonvar(M2), nonvar(X), X=<60,
    H2 > 1,
    H2 < 12,
    M3 is M2 - X,
    M3 < 0,
    M1 is M3 + 60,
    H1 is H2 - 1.
% subtracting minutes going to or over the hour, 12 am into 11 pm
timeAfterXBackward(pm(11, M1), am(12, M2), X) :-
    var(M1), nonvar(M2), nonvar(X), X=<60,
    M3 is M2 - X,
    M3 < 0,
    M1 is M3 + 60.
% subtracting minutes going to or over the hour, 12 am to 1 am
timeAfterXBackward(am(12, M1), am(1, M2), X) :-
    var(M1), nonvar(M2), nonvar(X), X=<60,
    M3 is M2 - X,
    M3 < 0,
    M1 is M3 + 60.

timeAfterXBackward(pm(A,B), pm(C,D), X) :-
    var(A), var(B), nonvar(C), nonvar(D), nonvar(X),
    timeAfterXBackward(am(A,B), am(C,D),X).

timeAfterXBackward(am(A,B), pm(C,D), X) :-
    var(A), var(B), nonvar(C), nonvar(D), nonvar(X),
    timeAfterXBackward(pm(A,B), am(C,D),X).

% no_midnight(T1,T2) is true if T2 is after T1 in the day (midnight is not between them)
no_midnight(am(A,B), am(A, C)) :- C > B.
no_midnight(am(A,_), am(B,_)) :- B > A, B \= 12.
no_midnight(am(12,_),am(B,_)) :- B < 12.
no_midnight(am(_,_), pm(_,_)).


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

%betweenTime(range(S, E), Time) if true if Time is in the range(S, E)
betweenTime(range(Time,_), Time).
betweenTime(range(_,Time), Time).
betweenTime(range(S, E), Time) :-
	beforeTime(S, Time),
	beforeTime(Time, E).

%betweenTimeNE(range(S, E), Time) if true if Time is in the range(S, E) not including the end time
betweenTimeNE(range(Time,_), Time).
betweenTimeNE(range(S, E), Time) :-
	beforeTime(S, Time),
	beforeTime(Time, E).
