:-[helper_preds].
:-[idx_interpolated_data].


% Finds the equivalent of an epsilon band on the above and below the band. this is for maxima.
% delta_band_max(++time, -list, -list).
delta(0.5).
delta_band_max(Date, BelowBand, AboveBand):-

    % get the epsilon band
    epsilon_band(Date, EpsBand),

    % get the next date above and below 
    last(EpsBand, Last),

    [First|_] = EpsBand,

    findall(D, (price(_Symbol,D,_,_,_,Close,_), date_offset(D, First, Offset),  Offset =< -1.0), LDates),
    reverse(LDates, RevLDates), % reverse so you iterate back through time
    [LDate|_] = RevLDates, % the date just after the epsilon band


    iterate_delta_below(RevLDates, LDate, BelowBand),


    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Last, Offset),  Offset >= 1.0), FDates),
    [FDate|_] = FDates, % The date just before the epsilon band

    iterate_delta_above(FDates, FDate, AboveBand).


% Find bands around an epsilon band. This predicate finds dates with similar closing prices above and below.
% This is for minima.
% delta_band_min(++time, -list, -list).
delta_band_min(Date, BelowBand, AboveBand):-

    % get the epsilon band
    epsilon_band(Date, EpsBand),

    % get the next date above and below 
    last(EpsBand, Last),

    [First|_] = EpsBand,

    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, First, Offset),  Offset =< -1.0), LDates),
    reverse(LDates, RevLDates), % reverse so you iterate back through time
    [LDate|_] = RevLDates, % the date just after the epsilon band

    iterate_delta_above(RevLDates, LDate, BelowBand),

    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Last, Offset),  Offset >= 1.0), FDates),
    [FDate|_] = FDates, % The date just before the epsilon band

    % for a min band, the above band is where you dont sell
    iterate_delta_below(FDates, FDate, AboveBand).


% base case: When there isonly one date in the list and it meets the condition
iterate_delta_below([X], _, [X]) :- !.

% main case to filter dates based on price difference
iterate_delta_below([D1|D1s], CompDate, [D1|Os]) :-
    delta(Del),
    price(_,_, D1, _, _, _, C1, _),
    price(_,_, CompDate, _, _, _, C2, _),
    Val is abs(C1 - C2),
    % Val is C1 - C2,

    (Val =< Del ->
        iterate_delta_below(D1s, CompDate, Os)
    ;
        Os = [] % Reset the list when the condition is not met
    ).

iterate_delta_above([X], _, [X]) :- !.

% main case to filter dates based on price difference
iterate_delta_above([D1|D1s], CompDate, [D1|Os]) :-
    delta(Del),
    price(_,_, D1, _, _, _, C1, _),
    price(_,_, CompDate, _, _, _, C2, _),
    Val is abs(C2 - C1),
    % Val is C2 - C1,

    (Val =< Del ->
        iterate_delta_above(D1s, CompDate, Os)
    ;
        Os = [] % reset the list when the condition is not met
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this fact determines the range for which epsilon is calculated.
epsilon(0.5).


% Find the epsilon band of an date, an epsilon band is a consecutive list of times extending above and 
% below a date containing dates with similar closing values. 
% It is useful for finding a range of dates around a maximum or minimum.
% epsilon_band(++time, -band).
epsilon_band(Date, Band):-
    % get all dates below "Date", including "Date"
    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset =< 0), DatesBel),

    % reverse the dates, we are comparing dates in the past from "Date" therefore when we iterate its
    % better to reverse to take advanatge of prolog's [Head|Tail] notation.
    reverse(DatesBel, RevDates),

    % as we are comparing a date to the day previous to it, it makes sense to have a list one day
    % behind the other (for which to compare).
    [_|Rest] = RevDates,

    iterate_epsilon_below(RevDates, Rest, BandBelow),
    reverse(BandBelow, RevBandBelow),
    
    % Get all the dates above
    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset > 0), DatesAb),

    [_|AbRest] = DatesAb,

    iterate_epsilon_above(DatesAb, AbRest, BandAbove),

    append(RevBandBelow, BandAbove, Band).


iterate_epsilon_below([X], [], [X]).
% main case to get all the previous dates in the epsilon band :)
iterate_epsilon_below([D1|D1s], [D2|D2s], [O|Os]):-
    epsilon(Eps),
    price(_,_,D1,_,_,_,C1,_),
    price(_,_,D2,_,_,_,C2,_),
    Val is abs(C1-C2),
    Val < Eps,
    O = D1,
    iterate_epsilon_below(D1s, D2s, Os).

% case where the epsilon rule is broken.
% Im not entirely sure how this works but it does.
iterate_epsilon_below([_|D1s], [_|D2s], []):-
    iterate_epsilon_below(D1s, D2s, _).

iterate_epsilon_above([X], [], [X]).
% main case to get all the previous dates in the epsilon band :)
iterate_epsilon_above([D1|D1s], [D2|D2s], [O|Os]):-
    epsilon(Eps),
    price(_,_,D1,_,_,_,C1,_),
    price(_,_,D2,_,_,_,C2,_),
    Val is abs(C1-C2),
    Val < Eps,
    O = D1,
    iterate_epsilon_above(D1s, D2s, Os).

% case where the epsilon rule is broken.
% Im not entirely sure how this works but it does.
iterate_epsilon_above([_|D1s], [_|D2s], []):-
    iterate_epsilon_above(D1s, D2s, _).

%%%%%%%%%%

% epsilon_band(Date, Band):-
%     % get all dates below "Date", including "Date"
%     findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset =< 0), DatesBel),

%     % reverse the dates, we are comparing dates in the past from "Date" therefore when we iterate its
%     % better to reverse to take advanatge of prolog's [Head|Tail] notation.
%     reverse(DatesBel, RevDates),

%     % as we are comparing a date to the day previous to it, it makes sense to have a list one day
%     % behind the other (for which to compare).
%     [_|Rest] = RevDates,

%     iterate_epsilon_below(RevDates, Rest, BandBelow),
%     reverse(BandBelow, RevBandBelow),
    
%     % Get all the dates above
%     findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset > 0), DatesAb),

%     [_|AbRest] = DatesAb,

%     iterate_epsilon_above(DatesAb, AbRest, BandAbove),

%     append(RevBandBelow, BandAbove, Band).


% iterate_epsilon_below([X], [], [X]).
% % main case to get all the previous dates in the epsilon band :)
% iterate_epsilon_below([D1|D1s], [D2|D2s], [O|Os]):-
%     epsilon(Eps),
%     price(_,_,D1,_,_,_,C1,_),
%     price(_,_,D2,_,_,_,C2,_),
%     Val is abs(C1-C2),
%     Val < Eps,
%     O = D1,
%     iterate_epsilon_below(D1s, D2s, Os).

% % case where the epsilon rule is broken.
% % Im not entirely sure how this works but it does.
% iterate_epsilon_below([_|D1s], [_|D2s], []):-
%     iterate_epsilon_below(D1s, D2s, _).

% iterate_epsilon_above([X], [], [X]).
% % main case to get all the previous dates in the epsilon band :)
% iterate_epsilon_above([D1|D1s], [D2|D2s], [O|Os]):-
%     epsilon(Eps),
%     price(_,_,D1,_,_,_,C1,_),
%     price(_,_,D2,_,_,_,C2,_),
%     Val is abs(C1-C2),
%     Val < Eps,
%     O = D1,
%     iterate_epsilon_above(D1s, D2s, Os).

% % case where the epsilon rule is broken.
% % Im not entirely sure how this works but it does.
% iterate_epsilon_above([_|D1s], [_|D2s], []):-
%     iterate_epsilon_above(D1s, D2s, _).


write_bands(Ticker, Date):-
    epsilon_band(Date, EpsBand),
    delta_band_min(Date, DBmin1, DBmin2),
    % delta_band_max(Date, DBmax1, DM)
    tell('bands.py'),
    write('eps_band = '),
    indicies_of(Ticker, EpsBand, EpsBandIdx),

    print(EpsBandIdx),nl,
    write('del_band_bel = '),
    indicies_of(Ticker, DBmin1, DBmin1Idx),
    print(DBmin1Idx), nl,
    write('del_band_abo = '),
    indicies_of(Ticker, DBmin2, DBmin2Idx),
    print(DBmin2Idx), nl,
    told.