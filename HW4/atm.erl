%    Jennifer Salas 914329903
%    Spring 2018 ECS 140B
%
%    Homework 4 - ATM Module
%
%   This module is the library of functions for instantiating an atm. In
%   addition with the bank module, this moodule demonstrates concurrency,
%   fault tolerance, and functionalism.
%
%     loop/0 is the main process for the atm. It waits for messages and then
%     executes a series of functions based on the message received. It supports
%     calls for instantiating the cashsupply and the associated bank, checking
%     the cashsupply, withdrawing from an account, depositing to an account, and
%     checking an accoutn balance.
%
%   The following methods are internal and innaccessible through import
%
%     start/4 instantiates a term storage table and stores its starting cash
%     supply and the name and pid of its associated bank.
%
%     cashsupply/1 returns the amount of cash stored within the term storage
%     table.
%
%     withdraw/3 checks if there is sufficient cash in the cashsupply and calls
%     do_withdraw/4 if available, otherwise rejects the transaction.
%
%     do_withdraw/4 sends a message to the associated bank for a withdrawal
%     request if the transaction is successful, it "dispenses" money by removing
%     it from its cash supply, or alerts the account holder that the transaction
%     was declined.
%
%     deposit/2 alerts the bank that a customer has deposited cash. It does not
%     affect the cashsupply.
%
%     balance/2 requests a response from the bank with a customer's bank balance

-module(atm).
-export([loop/0]).

loop() ->
    receive
        {start, PID, Bank, BAL, ATM}    -> start(PID, Bank, BAL, ATM);
        {cashsupply, ATM}               -> cashsupply(ATM);
        {withdraw, ACC, AMT, ATM}       -> withdraw(ACC, AMT, ATM);
        {deposit, ACC, AMT, ATM}        -> deposit(ACC, AMT, ATM);
        {balance, ACC, ATM}             -> balance(ACC, ATM)
    end,
    loop().

start(PID, Bank, BAL, ATM) ->
    ets:new(ATM, [named_table]),
    ets:insert(ATM, {cash, BAL}),
    ets:insert(ATM, {bank, Bank, PID}),
    io:format("atm ~p started with ~p dollars cash available ~n", [ATM, BAL]).

cashsupply(ATM) ->
    [{cash, Cash}] = ets:lookup(ATM, cash),
    io:format("atm ~p has ~p dollars on hand~n", [ATM, Cash]).

withdraw(ACC, AMT, ATM) ->
    [{cash, Cash}] = ets:lookup(ATM, cash),
    Available = Cash > AMT,
    if  Available -> do_withdraw(ACC, AMT, ATM, Cash);
        true -> io:format("sorry, insufficient cash in this atm~n", [])
    end.

do_withdraw(ACC, AMT, ATM, Cash) ->
    [{bank, Bank, PID}] = ets:lookup(ATM, bank),
    PID ! {withdraw, ACC, AMT, Bank, self()},
    receive
        {Bank, true, Msg} ->
            io:format("account ~p now has ~p dollars~n", Msg),
            ets:insert(ATM, {cash, Cash - AMT});
        {Bank, false, Msg} ->
            io:format("sorry, account ~p has only ~p dollars~n", Msg)
    end.

deposit(ACC, AMT, ATM) ->
    [{bank, Bank, PID}] = ets:lookup(ATM, bank),
    PID ! {deposit, ACC, AMT, Bank, self()},
    receive
    {true, Msg} ->
        io:format("account ~p now has ~p dollars~n", Msg)
    end.

balance(ACC, ATM) ->
    [{bank, Bank, PID}] = ets:lookup(ATM, bank),
    PID ! {balance, ACC, Bank}.
