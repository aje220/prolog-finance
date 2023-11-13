:- use_module(library(date)).
:- use_module(library(dcg/high_order)).
:- [helper_preds].
:-[interpolated_data].
:-[idx_interpolated_data].


% Data type declarations.
time(X):- findall(Date, price(_,Date,_,_,_,_,_), Dates), member(X, Dates).
time_list([]).
time_list([X|L]) :- time(X), time_list(L).
action(buy).
action(sell).
timing(date).
timing(idx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple moving average.

% Get all sma values for the entire database
% sma(++atom, ++int, -list)
sma_all(Symbol, N, SMAs):-
    findall(D, price(_, Symbol,D,_,_,_,_,_), Dates),
    findall(Avg, (member(DD, Dates), sma(Symbol, DD, N, Avg)), SMAs).

% Calculate the Simple Moving Average (SMA) for a specific symbol, date, and N days.
% sma(++timing, ++atom, ++time, ++int, -float).
sma(date, Symbol, Date, N, Avg) :-
    findall(Close, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset < 0, Offset > -N), Closes),
    sum_list(Closes, Sum),
    Avg is Sum / N.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sma(idx, Symbol, Idx, N, Avg):-
    findall(Close, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Idx, Offset), Offset < 0, Offset > -N), Closes),
    sum_list(Closes, Sum),
    Avg is Sum/N.

idx_offset(Idx1, Idx2, Offset):-
    Offset is Idx1 - Idx2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks the SMA crossover of a single day. This holds to be true if Time is a date at
% which a buy signal crossover occurs.
% xsma(++timing, ++atom, ++time, ++int, ++int, ++action).
xsma(date, Symbol, Time, LargerP, SmallerP, buy):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,

    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,

    sma(date, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    sma(date, Symbol, DateAfter, LargerP, LargeAfterAvg),

    sma(date, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    sma(date, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg > SmallBeforeAvg,
    SmallAfterAvg > LargeAfterAvg.


% Checks the SMA crossover of a single day. This holds to be true if Time is a date at
% which a sell signal crossover occurs.
% xsma(++timing, ++atom, ++time, ++int, ++int, ++action).
xsma(date, Symbol, Time, LargerP, SmallerP, sell):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,
    sma(date, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    sma(date, Symbol, DateAfter, LargerP, LargeAfterAvg),

    sma(date, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    sma(date, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg < SmallBeforeAvg,
    SmallAfterAvg < LargeAfterAvg.


xsma(idx, Symbol, Time, LargerP, SmallerP, buy):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,

    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,

    sma(idx, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    sma(idx, Symbol, DateAfter, LargerP, LargeAfterAvg),

    sma(idx, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    sma(idx, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg > SmallBeforeAvg,
    SmallAfterAvg > LargeAfterAvg.


% Checks the SMA crossover of a single day. This holds to be true if Time is a date at
% which a sell signal crossover occurs.
% xsma(++timing, ++atom, ++time, ++int, ++int, ++action).
xsma(idx, Symbol, Time, LargerP, SmallerP, sell):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,
    findall(I, (price(I,Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,
    sma(idx, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    sma(idx, Symbol, DateAfter, LargerP, LargeAfterAvg),

    sma(idx, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    sma(idx, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg < SmallBeforeAvg,
    SmallAfterAvg < LargeAfterAvg.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exponential moving average

% Calculate all the EMA values for every fact in the database.
% ema_all(++atom, ++int, -list).
ema_all(Symbol, N, EMAs):-
    findall(D, price(_,Symbol, D, _, _, _, _, _), Dates),
    last(Dates, Date),
    emas(Symbol, Date, N, EMAs).

% Calculate the emas up to a certain date.
% emas(++timing, ++atom, ++time, ++int, -list).
emas(date, Symbol, Date, N, EMAs) :-
    findall(Close, (price(_, Symbol, D, _, _, _, Close, _), date_offset(D, Date, Offset), Offset < 0), Closes),
    Smoothing is 2 / (N + 1),
    Closes = [C|Cs],
    calculate_ema_values(Cs, C, Smoothing, EMAs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emas(idx, Symbol, Date, N, EMAs) :-
    findall(Close, (price(I, Symbol, _, _, _, _, Close, _), idx_offset(I, Date, Offset), Offset < 0), Closes),
    Smoothing is 2 / (N + 1),
    Closes = [C|Cs],
    calculate_ema_values(Cs, C, Smoothing, EMAs).

ema(idx, Symbol, I, N, EMA):-
    emas(idx, Symbol, I, N, EMAs),
    last(EMAs, EMA).

% Calculate the ema of a single date.
% ema(++timing, ++atom, ++time, ++int, -list)
ema(date, Symbol, Date, N, EMA) :-
    emas(date, Symbol, Date, N, EMAs),
    last(EMAs, EMA).

% Inticator trigger for EMA crossover. Holds when a buy signal is found at date "Time".
% xema(++timing, ++atom, ++time, ++int, ++int, ++action).
xema(date, Symbol, Time, LargerP, SmallerP, buy):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,
    ema(date, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    ema(date, Symbol, DateAfter, LargerP, LargeAfterAvg),

    ema(date, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    ema(date, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg > SmallBeforeAvg,
    SmallAfterAvg > LargeAfterAvg.

% Inticator trigger for EMA crossover. Holds when a sell signal is found at date "Time".
% xema(++timing, ++atom, ++time, ++int, ++int, ++action).
xema(date, Symbol, Time, LargerP, SmallerP, sell):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,
    findall(D, (price(_, Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,
    ema(date, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    ema(date, Symbol, DateAfter, LargerP, LargeAfterAvg),

    ema(date, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    ema(date, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg < SmallBeforeAvg,
    SmallAfterAvg < LargeAfterAvg.


% Inticator trigger for EMA crossover. Holds when a buy signal is found at date "Time".
% xema(++timing, ++atom, ++time, ++int, ++int, ++action).
xema(idx, Symbol, Time, LargerP, SmallerP, buy):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,
    ema(idx, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    ema(idx, Symbol, DateAfter, LargerP, LargeAfterAvg),

    ema(idx, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    ema(idx, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg > SmallBeforeAvg,
    SmallAfterAvg > LargeAfterAvg.

% Inticator trigger for EMA crossover. Holds when a sell signal is found at date "Time".
% xema(++timing, ++atom, ++time, ++int, ++int, ++action).
xema(idx, Symbol, Time, LargerP, SmallerP, sell):-
    LargerP > SmallerP,
    % get the next date above and below
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,
    findall(I, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,
    ema(idx, Symbol, DateBefore, LargerP, LargeBeforeAvg),
    ema(idx, Symbol, DateAfter, LargerP, LargeAfterAvg),

    ema(idx, Symbol, DateBefore, SmallerP, SmallBeforeAvg),
    ema(idx, Symbol, DateAfter, SmallerP, SmallAfterAvg),

    LargeBeforeAvg < SmallBeforeAvg,
    SmallAfterAvg < LargeAfterAvg.

% Calculate EMA values for the list of closing prices
calculate_ema_values([], _, _, []).
calculate_ema_values([Close | RestCloses], CurrentEMA, Multiplier, [NewEMA | RestEMAs]) :-
    NewEMA is (Close - CurrentEMA) * Multiplier + CurrentEMA,
    calculate_ema_values(RestCloses, NewEMA, Multiplier, RestEMAs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RSI

% Calculate the RSI values for all items in the database.
% rsi_all(++atom, ++date, -list)
rsi_all(Symbol, N, RSIs):-
    findall(D, price(Symbol,D,_,_,_,_,_), Dates),
    findall(RSI, (member(DD, Dates), rsi(Symbol, DD, N, RSI)), RSIs).

% Calculate RSI for a single date.
% rsi(++timing, ++Atom, ++Time, ++Int, -Float).
rsi(date, Symbol, Date, N, RSI):-
    average_gain(date, Symbol, Date, N, AvgGain),
    average_loss(date,Symbol, Date, N, AvgLoss),
    (AvgLoss is 0 -> AdjustedAvgLoss = 0.0001; AdjustedAvgLoss = AvgLoss),
    RS is AvgGain/AdjustedAvgLoss,
    RSI is 100 - (100/(1+RS)).

rsi(idx, Symbol, Date, N, RSI):-
    average_gain(idx, Symbol, Date, N, AvgGain),
    average_loss(idx, Symbol, Date, N, AvgLoss),
    (AvgLoss is 0 -> AdjustedAvgLoss = 0.0001; AdjustedAvgLoss = AvgLoss),
    RS is AvgGain/AdjustedAvgLoss,
    RSI is 100 - (100/(1+RS)).

% Average gain calculated over a back period.
% average_gain(++timing, ++atom, ++time, ++int, -Float).
average_gain(date, Symbol, Date, N, AvgGain):-
    % get all the prices in the back days
    findall(Close, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset < 0, Offset >= -N), Closes),
    reverse(Closes, RCloses),
    calculate_gains(RCloses, Gains),
    sum_list(Gains, Sum),
    AvgGain is Sum / N.

average_gain(idx, Symbol, Date, N, AvgGain):-
    % get all the prices in the back days
    findall(Close, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Date, Offset), Offset < 0, Offset >= -N), Closes),
    reverse(Closes, RCloses),
    calculate_gains(RCloses, Gains),
    sum_list(Gains, Sum),
    AvgGain is Sum / N.


% Average loss calculated over a back period.
% average_loss(++timing, ++atom, ++time, ++int, -float).
average_loss(date, Symbol, Date, N, AvgLoss):-
    % get all the prices in the back days
    findall(Close, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset < 0, Offset >= -N), Closes),
    reverse(Closes, RCloses),
    calculate_losses(RCloses, Losses),
    sum_list(Losses, Sum),
    AvgLoss is Sum / N.

% Average loss calculated over a back period.
% average_loss(++timing, ++atom, ++time, ++int, -float).
average_loss(idx, Symbol, Date, N, AvgLoss):-
    % get all the prices in the back days
    findall(Close, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Date, Offset), Offset < 0, Offset >= -N), Closes),
    reverse(Closes, RCloses),
    calculate_losses(RCloses, Losses),
    sum_list(Losses, Sum),
    AvgLoss is Sum / N.


% Predicate that holds if RSI signals that the asset is oversold at date "Date".
% N is the period. Tol is the tolerance, should be a low value, like 20.
% oversold(++timing, ++atom, ++time, ++int, ++float)
rsi_oversold(date, Symbol, Date, N, Tol):-
    rsi(date, Symbol, Date, N, Rsi),
    Rsi < Tol.

rsi_oversold(idx, Symbol, Date, N, Tol):-
    rsi(idx, Symbol, Date, N, Rsi),
    Rsi < Tol.

% Predicate that holds if RSI signals that the asset is overbought at date "Date".
% N is the period. Tol is the tolerance, should be a high value, like 80.
% overbought(++timing, ++atom, ++time, ++int, ++float)
rsi_overbought(date, Symbol, Date, N, Tol):-
    rsi(Symbol, Date, N, Rsi),
    Rsi > Tol.

rsi_overbought(idx, Symbol, Date, N, Tol):-
    rsi(idx, Symbol, Date, N, Rsi),
    Rsi > Tol.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stochastic occillator.


% Calculate fast k of date "Date".
% fast_k(++timing, ++atom, ++time, ++int, -FastK).
fast_k(date, Symbol, Date, N, FastK):-
    price(_,Symbol,Date,_,_,_,CurrentClose,_),
    findall(Close, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Date, Offset), Offset < 0, Offset >= -N), Closes),
    min_list(Closes, Min),
    max_list(Closes, Max),
    Num is CurrentClose - Min,
    Denom is (Max - Min),
    (Denom is 0.0 -> AdjustedDenom = 0.0001; AdjustedDenom = Denom),
    FastK is (Num/AdjustedDenom)*100.


fast_k(idx, Symbol, Date, N, FastK):-
    price(Date, Symbol,_,_,_,_,CurrentClose,_),
    findall(Close, (price(I, Symbol,_,_,_,_,Close,_), idx_offset(I, Date, Offset), Offset < 0, Offset >= -N), Closes),
    min_list(Closes, Min),
    max_list(Closes, Max),
    Num is CurrentClose - Min,
    Denom is (Max - Min),
    (Denom is 0.0 -> AdjustedDenom = 0.0001; AdjustedDenom = Denom),
    FastK is (Num/AdjustedDenom)*100.


% N is the backlog from which the fast k is calculated, Smoothing period is the period for the moving average
% Example N=14, SmoothingPeriod=3. You go back 14 days to calc fastK, then do that for 3 days previous and 
% take average.
% Calculate the slow_k of date "Date".
% slow_k(++timing, ++atom, ++time, ++int, ++int, -float).
slow_k(date, Symbol, Date, N, SmoothingPeriod, SlowK):-
    % get the dates in the for the smoothing period
    findall(D, (price(_,Symbol,D,_,_,_,_,_), date_offset(D, Date, Offset), Offset < 0, Offset >= -SmoothingPeriod), Dates),
    % get fastK of of all
    findall(K, (member(DD, Dates), fast_k(date, Symbol, DD, N, K)), FastKs),
    % average
    sum_list(FastKs, Sum),
    SlowK is Sum/SmoothingPeriod.


slow_k(idx, Symbol, Date, N, SmoothingPeriod, SlowK):-
    % get the dates in the for the smoothing period
    findall(I, (price(I, Symbol,_,_,_,_,_,_), idx_offset(I, Date, Offset), Offset < 0, Offset >= -SmoothingPeriod), Dates),
    % get fastK of of all
    findall(K, (member(DD, Dates), fast_k(idx, Symbol, DD, N, K)), FastKs),
    % average
    sum_list(FastKs, Sum),
    SlowK is Sum/SmoothingPeriod.

% N is for the initial fastK, Smoothing period is the period of which you take the SMA of fastKs.
% This takes the SMA of the slowKs, for 3 days. 
% Calculate SlowD.
% slow_d(++timing, ++atom, ++time, ++int, ++int, -float).
slow_d(date, Symbol, Date, N, SmoothingPeriod, SlowD):-
    
    % last three days
    findall(D, (price(_,Symbol,D,_,_,_,_,_), date_offset(D, Date, Offset), Offset < 0, Offset >= -3), Dates),

    % get slowK of all
    findall(K, (member(DD, Dates), slow_k(date, Symbol, DD, N, SmoothingPeriod, K)), SlowKs),

    sum_list(SlowKs, Sum),
    SlowD is Sum/3.

slow_d(idx, Symbol, Date, N, SmoothingPeriod, SlowD):-
    
    % last three days
    findall(I, (price(I, Symbol,_,_,_,_,_,_), idx_offset(I, Date, Offset), Offset < 0, Offset >= -3), Dates),

    % get slowK of all
    findall(K, (member(DD, Dates), slow_k(idx, Symbol, DD, N, SmoothingPeriod, K)), SlowKs),

    sum_list(SlowKs, Sum),
    SlowD is Sum/3.


stoch_max_tol(80).
stoch_min_tol(20).
stochastic_X(date, Symbol, Time, N, SmoothingPeriod, buy):-
    % slowk and slowd
    stoch_max_tol(X),
    % print(Time),

    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,
    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,

    slow_k(date, Symbol, DateBefore, N, SmoothingPeriod, BefSlowK),
    slow_k(date, Symbol, DateAfter, N, SmoothingPeriod, AftSlowK),

    slow_d(date, Symbol, DateBefore, N, SmoothingPeriod, BefSlowD),
    slow_d(date, Symbol, DateAfter, N, SmoothingPeriod, AftSlowD),

    BefSlowD > X,
    AftSlowD > X,
    BefSlowD > X,
    AftSlowD > X,

    AftSlowD > AftSlowK,
    BefSlowD > BefSlowK.

stochastic_X(idx, Symbol, Time, N, SmoothingPeriod, buy):-
    % slowk and slowd
    % print(Time),

    
    stoch_max_tol(X),
    findall(I, (price(I,Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,
    findall(I, (price(I,Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,

    slow_k(idx, Symbol, DateBefore, N, SmoothingPeriod, BefSlowK),
    slow_k(idx, Symbol, DateAfter, N, SmoothingPeriod, AftSlowK),

    slow_d(idx, Symbol, DateBefore, N, SmoothingPeriod, BefSlowD),
    slow_d(idx, Symbol, DateAfter, N, SmoothingPeriod, AftSlowD),

    BefSlowD > X,
    AftSlowD > X,
    BefSlowD > X,
    AftSlowD > X,

    AftSlowD > AftSlowK,
    BefSlowD > BefSlowK.

stochastic_X(date, Symbol, Time, N, SmoothingPeriod, sell):-
    % slowk and slowd
    % print(Time),
    
    stoch_max_tol(X),
    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
    [DateBefore|_] = DateBL,
    findall(D, (price(_,Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
    [DateAfter|_] = DatesAL,

    slow_k(date, Symbol, DateBefore, N, SmoothingPeriod, BefSlowK),
    slow_k(date, Symbol, DateAfter, N, SmoothingPeriod, AftSlowK),

    slow_d(date, Symbol, DateBefore, N, SmoothingPeriod, BefSlowD),
    slow_d(date, Symbol, DateAfter, N, SmoothingPeriod, AftSlowD),

    BefSlowD < X,
    AftSlowD < X,
    BefSlowD < X,
    AftSlowD < X,

    AftSlowD < AftSlowK,
    BefSlowD < BefSlowK.

stochastic_X(idx, Symbol, Time, N, SmoothingPeriod, sell):-
    % slowk and slowd
    
    stoch_max_tol(X),
    findall(I, (price(I,Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = -5), DateBL),
    [DateBefore|_] = DateBL,
    findall(I, (price(I,Symbol,_,_,_,_,Close,_), idx_offset(I, Time, Offset), Offset = 5), DatesAL),
    [DateAfter|_] = DatesAL,

    slow_k(idx, Symbol, DateBefore, N, SmoothingPeriod, BefSlowK),
    slow_k(idx, Symbol, DateAfter, N, SmoothingPeriod, AftSlowK),

    slow_d(idx, Symbol, DateBefore, N, SmoothingPeriod, BefSlowD),
    slow_d(idx, Symbol, DateAfter, N, SmoothingPeriod, AftSlowD),

    BefSlowD < X,
    AftSlowD < X,
    BefSlowD < X,
    AftSlowD < X,

    AftSlowD < AftSlowK,
    BefSlowD < BefSlowK.


% stochastic_X(Symbol, Time, N, SmoothingPeriod, sell):-
%     % slowk and slowd
    
%     stoch_min_tol(X),
%     findall(D, (price(Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = -5.0), DateBL),
%     [DateBefore|_] = DateBL,
%     findall(D, (price(Symbol,D,_,_,_,Close,_), date_offset(D, Time, Offset), Offset = 5.0), DatesAL),
%     [DateAfter|_] = DatesAL,

%     slow_k(Symbol, DateBefore, N, SmoothingPeriod, BefSlowK),
%     slow_k(Symbol, DateAfter, N, SmoothingPeriod, AftSlowK),

%     slow_d(Symbol, DateBefore, N, SmoothingPeriod, BefSlowD),
%     slow_d(Symbol, DateAfter, N, SmoothingPeriod, AftSlowD),

%     BefSlowD < X,
%     AftSlowD < X,
%     BefSlowD < X,
%     AftSlowD < X,

%     AftSlowD < AftSlowK,
%     BefSlowD < BefSlowK.


% Three predicates to calculate the stochasitic indicators for all facts in the datebase.
% fast_k_all(++atom, ++int, -list).
fast_k_all(Symbol, N, FastKs):-
    findall(D, price(_,Symbol,D,_,_,_,_,_), Dates),
    findall(FastK, (member(DD, Dates), fast_k(Symbol, DD, N, FastK)), FastKs).

% slow_k_all(++atom, ++int, ++int, -list).
slow_k_all(Symbol, N, SmoothingPeriod, SlowKs):-
    findall(D, price(_,Symbol,D,_,_,_,_,_), Dates),
    findall(SlowK, (member(DD, Dates), slow_k(Symbol, DD, N, SmoothingPeriod, SlowK)), SlowKs).

% slow_d_all(++atom, ++int, ++int, -list).
slow_d_all(Symbol, N, SmoothingPeriod, SlowDs):-
    findall(D, price(_,Symbol,D,_,_,_,_,_), Dates),
    findall(SlowD, (member(DD, Dates), slow_d(Symbol, DD, N, SmoothingPeriod, SlowD)), SlowDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bollinger Bands

% Calculate the Bollinger band of a single date "Date"
% bollinger_band(Symbol, Date, N, Mean, Upper, Lower)
bollinger_band(date, Symbol, Date, N, Mean, Upper, Lower):-
    % get current price
    price(_,Symbol, Date,_,_,_,CurrentClose, _),
    sma(date, Symbol, Date, N, Mean), % calc sma/mean

    boll_std(Mean, CurrentClose, N, Std),
    Upper is Mean + (2*Std),
    Lower is Mean - (2*Std).

bollinger_band(idx, Symbol, Date, N, Mean, Upper, Lower):-
    % get current price
    price(Date, Symbol, _,_,_,_,CurrentClose, _),
    sma(idx, Symbol, Date, N, Mean), % calc sma/mean

    boll_std(Mean, CurrentClose, N, Std),
    Upper is Mean + (2*Std),
    Lower is Mean - (2*Std).

% Helper predicate to get the standard deviation for Bollinger Bands.
% boll_std(++float, ++float, ++int, -float).
% boll_std(Avg, CurrentClose, N, Std):-
%     S is (CurrentClose - Avg)**2,
%     sum_list([S], Sum),
%     Variance is Sum/N,
%     Std is sqrt(Variance).

% Helper predicate to get the standard deviation for Bollinger Bands.
% boll_std(++float, ++float, ++int, -float).
boll_std(Avg, CurrentClose, N, Std):-
    S is (CurrentClose - Avg)**2,
    Variance is S/N,
    Std is sqrt(Variance).


% Get the bollinger bands of all the entires in the database.
% boll_all(++atom, ++period, -bands).
boll_all(Symbol, N, Bands):-
    findall(D, price(Symbol,D,_,_,_,_,_), Dates),
    findall([Mean, Upper, Lower], (member(DD, Dates), bollinger_band(Symbol, DD, N, Mean, Upper, Lower)), Bands).

% Holds when the value at "Date" pierces the lower band, N the back period from which the 
% mean is calculated.
% boll_oversold(++atom, +time, ++int).
boll_oversold(date, Symbol, Date, N):-
    bollinger_band(date, Symbol, Date, N, _, _, Lower),
    price(_, Symbol, Date, _, _, _, C, _),
    C =< Lower.

boll_oversold(idx, Symbol, Date, N):-
    bollinger_band(idx, Symbol, Date, N, _, _, Lower),
    price(Date, Symbol, _, _, _, _, C, _),
    C =< Lower.

% Holds when the value at "Date" pierces the upepr band, N the back period from which the 
% mean is calculated.
% boll_overbought(++atom, +time, ++int).
boll_overbought(date, Symbol, Date, N):-
    bollinger_band(date, Symbol, Date, N, _, Upper, _),
    price(_, Symbol, Date, _, _, _, C, _),
    C >= Upper.

boll_overbought(idx, Symbol, Date, N):-
    bollinger_band(idx, Symbol, Date, N, _, Upper, _),
    price(Date, Symbol, _, _, _, _, C, _),
    C >= Upper.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
