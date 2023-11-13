


indicies_of(_, [], []).
indicies_of(Symbol, [T|Ts], [I|Is]):-
    findall(D, (price(_, Symbol, D, _, _, _, _, _), date_offset(D, T, Offset), Offset < 0), Dates), % less than zero so it indexes at zero
    length(Dates, I),
    % print(I),
    indicies_of(Symbol, Ts, Is).


calculate_gains([_],[]).
calculate_gains([C|Cs], [G|Gs]):-
    Cs = [P|_],
    % print(P),
    Diff is C - P,
    % nl,
    % print(Diff), nl,
    (Diff >= 0 -> G is Diff; G is 0),
    calculate_gains(Cs, Gs).

calculate_losses([_],[]).
calculate_losses([C|Cs], [G|Gs]):-
    Cs = [P|_],
    % print(P),
    Diff is C - P,
    % nl,
    % print(Diff), nl,
    (Diff =< 0 -> G is abs(Diff); G is 0),
    calculate_losses(Cs, Gs).



% Helper predicate to calculate the offset date, this works one way, as in we can find the 
% days difference between two dates.
% date_offset(++time, ++time, -float).
date_offset(Date1, Date2, N) :-
    parse_date(Date1, Y1,M1,D1),
    % print(Date1),
    parse_date(Date2, Y2,M2,D2),
    % print(Date2),
    atom_number(Y1, Y1Num),
    atom_number(M1, M1Num),
    atom_number(D1, D1Num),
    atom_number(Y2, Y2Num),
    atom_number(M2, M2Num),
    atom_number(D2, D2Num),
    date_offset_days(date(Y1Num,M1Num,D1Num), date(Y2Num,M2Num,D2Num), N).

% Predicate that computes the number of days between two dates, 
% the difference between the first and second date, can give negative numbers
date_offset_days(date(Y1, M1, D1), date(Y2, M2, D2), N) :-
    date_time_stamp(date(Y1, M1, D1, 0, 0, 0, 0, _, _), Stamp1),
    date_time_stamp(date(Y2, M2, D2, 0, 0, 0, 0, _, _), Stamp2),
    Diff is Stamp1 - Stamp2,
    % print(Diff),
    % N is abs(Diff / 86400).
    N is Diff / 86400.


% Helper rules to parse the dates in the stock facts database.
parse_date(Date, Y, M, D) :-
    nonvar(Date),
    atomic_list_concat([Y, M, D], '-', Date).

parse_date(Date, Y, M, D) :-
    nonvar(Y),
    nonvar(M),
    nonvar(D),
    atomic_list_concat([Y, M, D], '-', Date).