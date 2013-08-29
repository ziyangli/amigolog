%% consult the top-level interpreter
:- ['../../interpreter/indigolog'].

%% consult the owl parser
%% :- use_module("~/ros/workspace/git/thea/owl2_io.pl").
%% :- use_module("~/ros/workspace/git/thea/owl2_model.pl").

%% consult application
%% :- consult(demo_waiter).
:- consult(amigo_action_domain).
%% :- consult(demo_elevator).

%% consult testing file
%% :- consult(util_test).

%% -- main
main :-	indigolog(waiter_program).

%% -- trace_setting
trace_setting :-
%%       spy(mayEvolve),
%%       spy(final),
        trace(fails),
        trace(thread_get_message),
        spy(indixeq), 
        trace(mayEvolve),
        trace(indigo2), 
        trace(holds, -all), 
        trace(trans),
        trace(final),
        trace(has_val/3, -all),
        trace(initially/2, -all).
%% :- trace_setting.

