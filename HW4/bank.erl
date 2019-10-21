%    Jennifer Salas 914329903
%    Spring 2018 ECS 140B
%
%    Homework 4 - Bank Module
%
%   This module is the library of functions for instantiating a Bank. In
%   addition with the atm module, this moodule demonstrates concurrency,
%   fault tolerance, and functionalism.
%
%     loop/0 is the main process for the atm. It waits for messages and then
%     executes a series of functions based on the message received. It supports
%     calls for instantiating a series of accounts in a database, creating a
%     new account, checking a account's balance, depositing into an
%     account, or withdrawing from an account.
%
%   The following methods are internal and innaccessible through import
%
%     create/2 generates the bank term storage and inserts a series of accounts,
%     if any are inserted.
%
%     open/3 adds an account to the bank term storage with a starting balance.
%
%     balance/2 checks the balance of account against the bank term storage
%     and prints a message with the stored balance.
%
%     deposit/3 overwrites the balance of an account in term storage.
%
%     withdraw/4 checks if there is a sufficient balance in an account and then
%     sends a message that a withdrawal was either successful or failed.

-module(bank).
-export([loop/0]).

loop() ->
    receive
        {create, ACCS, Bank}            -> create(ACCS, Bank);
        {open, ACC, BAL, Bank}          -> open(ACC, BAL, Bank);
        {balance,ACC, Bank}             -> balance(ACC, Bank);
        {deposit, ACC, AMT, Bank, ATM}  -> deposit(ACC, AMT, Bank, ATM);
        {withdraw, ACC, AMT, Bank, ATM} -> withdraw(ACC, AMT, Bank, ATM)
    end,
    loop().

create(ACCS, Bank) ->
    ets:new(Bank, [named_table]),
    ets:insert(Bank, {bank, Bank}),
    lists:map(fun(I) -> ets:insert(Bank,I) end, ACCS),
    io:format("bank ~p created~n", [Bank]).

open(ACC, BAL, Bank) ->
    ets:insert(Bank, {ACC, BAL}),
    io:format("new account ~p opened with ~p dollars~n", [ACC, BAL]).

balance(ACC, Bank) ->
    [{ACC, BAL}] = ets:lookup(Bank, ACC),
    io:format("account ~p has ~p dollars~n", [ACC, BAL]).

deposit(ACC, AMT, Bank, ATM) ->
    [{ACC, BAL}] = ets:lookup(Bank, ACC),
    ets:insert(Bank, {ACC, BAL + AMT}),
    Msg = [ACC, BAL + AMT],
    ATM ! {true, Msg}.

withdraw(ACC, AMT, Bank, ATM) ->
    [{ACC, BAL}] = ets:lookup(Bank, ACC),
    Success = BAL >= AMT,
    if  Success ->
            ets:insert(Bank, {ACC, BAL - AMT}),
            Msg = [ACC, BAL - AMT];
        true    -> Msg = [ACC, BAL]
    end,
    ATM ! {Bank, Success, Msg}.
