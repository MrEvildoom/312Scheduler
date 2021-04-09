scheduleList(_, [], []).
scheduleList(AvailableTimes, [UnschH|UnschT], [SchH|SchT]) :- % If no prerequisites, schedule in first available time
    noPrerequisite(UnschH), % TODO: probably can be done with pattern matching
    nextAvailable(AvailableTimes, NextAvailable, RemainingTimes),
    SchH is scheduledTask(UnschH, NextAvailable),
    scheduleList(RemainingTimes, UnschT, SchT).

scheduleList(AvailableTimes, [UnschH|UnschT], SchList) :-
    getPrereq(UnschH, HPrereq),
    reorderList(HPrereq, UnschT, [HPrqH|ReorderedList]),
    scheduleList(AvailableTimes, [HPrqH, UnschH|ReorderedList], SchList).


noPrerequisite(task())