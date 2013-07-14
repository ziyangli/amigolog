:- style_check(-discontiguous). %% disable warning different location of definition
:- set_prolog_flag(backquoted_string, true). %% set ` to be the string construct
:- set_prolog_flag(optimise, true).

:- multifile
        prim_action/1,
        causes_val/4,
        poss/2,
        proc/2,
        prim_fluent/1.

%% Consult the top-level interpreter
%% :- ['$PATH_INDIGOLOG/interpreter/indigolog-vanilla_swi'].
%% Consult the threaded interpreter
:- ['../../interpreter/indigolog'].

:- set_debug_level(warn_on).

%% Consult application
%% :- consult(demo_waiter).
:- consult(amigo_action_domain).
:- consult(demo_elevator).
%% :- use_module("~/ros/workspace/git/thea/owl2_io.pl").
%% :- use_module("~/ros/workspace/git/thea/owl2_model.pl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% main/0: Gets IndiGolog to evaluate a chosen mainControl procedure
main :-
	indigolog(waiter_program).

util_debug :- thread_signal(indigolog_thread, (attach_console, trace)).

util_next :- thread_send_message(indigolog_thread, next_step).

util_exog :- thread_send_message(indigolog_thread, request(table(4), 3)).

util_feedback(Result) :-
        executing_action(A),
        is_list(A),
        A = [A1|A2], 
        thread_send_message(indigolog_thread, got_sensing(A1, Result)), 
        thread_send_message(indigolog_thread, got_sensing(A2, Result)), !.

util_feedback(Result) :-
        executing_action(Act),
        thread_send_message(indigolog_thread, got_sensing(Act, Result)), !.

util_exog(ExogAct) :-
        exog_action_occurred([ExogAct]).


        
% prim_fluent(robotDir).

% prim_fluent(today).
% currently(today, wednesday).

% prim_fluent(light(1)).
% prim_fluent(light(2)).
% prim_fluent(light(3)).
% currently(light(1), on).
% currently(light(2), off).
% currently(light(3), on).
% cache(light(_)).

roll_parameters(3, 5, 2).

prim_fluent(test_counter).
initially(test_counter, 0).
%% currently(test_counter, 0).
cache(test_counter).

prim_fluent(counter_2).
initially(counter_2, 100).


prim_action(add_count).
poss(add_count, test_counter<1000).
causes_val(add_count, test_counter, V, V is test_counter+1).
causes_val(add_count, counter_2, V, V is counter_2-1).

proc(on_off_combine_test,
     [search([star(add_count, 10), ?(test_counter=4), go_table(1)], 'searching message.'),
      go_table(4)]).


proc(trans_final_test, [star(add_count), star(go_table(1)), star(go_table(2))]).
% kukudm(a, b).
% kukudm(a, c).
% kukudm(a, d).

% kuku2(b, 44).
% kuku2(b, 55).
% kuku2(c, 99).


% :- dynamic hahaha/4.
% kuku_test(A, F) :-
%         kukudm(C, D),
%         ( \+ hahaha(A, F, C, D) -> assert(hahaha(A, F, C, D)); true),
%         fail.
% kuku_test(_, _).

% show_kuku(A, V) :- kukudm(A, B), kuku2(B, V).

% kuku_test2(I) :- show_kuku(a, V), V=I.

trace_setting :-
%       spy(mayEvolve),
%       spy(final), 
        trace(mayEvolve),
        trace(indigo2), 
        trace(holds, -all), 
        trace(trans),
        trace(final),
        trace(has_val/3, -all),
        trace(initially/2, -all).
%% :- trace_setting.


prim_action(del_count(X)) :- member(X, [1, 2, 3]).
poss(del_count, false).
proc(ndet_test, ndet([add_count, del_count], [add_count, add_count])).
proc(rpi_test, rpi(X, [1, 2, 3], star(add_count(X), 1))).
proc(star_test, star(add_count, 1)).

proc(roll_action_test, [add_count, add_count, add_count, add_count, add_count, add_count, add_count]).
proc(par_test, [add_count, par(add_count, add_count), add_count]).