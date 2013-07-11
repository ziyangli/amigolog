%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : indigolog-vanilla.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  In addition to a (Con)Golog program, users provide these predicates:
%
%      prim_fluent(fluent),             for each primitive fluent
%      prim_action(action),             for each primitive action
%      exog_action(action),             for each exogenous action
%      senses(action,fluent),           for each sensing action
%      poss(action,cond)                when cond, action is executable
%      initially(fluent,value)          fluent has value in S0
%      causes_val(action,fluent,value,cond)
%            when cond holds, doing act causes fluent to have value
%
%      execute(action,sensing_result)   do the action, return the result
%            can use ask_execute
%      exog_occurs(action)              return an exog action
%            can use ask_exog_occurs (or fail, if none)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
        senses/1,
        senses/5, 
        senses/2,
        indi_exog/1, % store exog events not managed yet
        now/1,       % store actual history
        rollednow/1, % part of th actual history that has been rolled
        wait_at_action/1, % wait some seconds after EACH action
        
        protectHistory/1, % TODO: protect a history to avoid rolling
        pause_step/0. % flag to show a step is paused
        
:-
        multifile(set_option/1),
        multifile(set_option/2),
        multifile(exog_action/1),
        multifile(system_action/1).
        
%% added by ziyang
:-dynamic
        indigolog_plan/1, 
        indigolog_action/1, 
        num_of_actions/1.


assertz_plan(Plan) :-
	retractall(indigolog_plan(_)), retractall(num_of_actions(_)),
	%% reverse(Plan, Plan_rev),
	assertz(indigolog_plan(Plan)),
	length(Plan, NOA), assertz(num_of_actions(NOA)).

read_action(Act) :-
	thread_get_message(Act).

plan_done(ID) :- write('Plan Done and '), write(ID), write(' exited!').

reset_indigolog_dbs :-
        retractall(doing_step),
        retractall(indi_exog(_)),
        %% retractall(protectHistory(_)),
        retractall(rollednow(_)),
        retractall(now(_)),
        update_now([]),
        assert(rollednow([])).

initialize :-
        retractall(currently(_, _)),
        forall(initially(F, V), assert(currently(F, V))), 
        clean_cache, %% clean has_valc/2
        reset_indigolog_dbs,
        refresh_counter.

finalize.

%% Mapping
:- dynamic
        settles/5,
        rejects/5.

%%    causes_val like predicates, define effects of sensing actions
%% -- rejects(+SensingAct, +SensedResult, +Fluent, +Value, +Condition)
%%    If Condition holds and SensingAct gets SensedResult, then Fluent does not
%%    get Value
%%    reject(smell, 0, locWumpus, Y, adj(locRobot, Y)) means if smell gets 0 as result,
%%    then ALL the adjcent location Y of locRobot can not be location of the Wumpus.
%% -- settles(+SensingAct, +SensedResult, +Fluent, +Value, +Condition)
%%    settles(senseGold, 1, isGold(L), true, L=locRobot).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(data_util).
%% :- ensure_loaded(sysexog). %% load the system exog events
:- ensure_loaded(sys_util).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TOP LEVEL MAIN CYCLE
%% -- indigolog(E)
%%    E is a high-level program
%%    The history H is a list of actions (prim or exog), initially [] (empty)
%%    Sensing reports are inserted as actions of the form e(fluent,value)

:- dynamic
        doing_step/0. %% flag to show a step is being calculated

%% -- predicate to prepare everything for computing the next single step.
%%    diable gc to speed up
prepare_for_step :- turn_off_gc. %% before computing a step
wrap_up_step :-                  %% after computing a step
        turn_on_gc,
        garbage_collect.

%% -- abortStep
%%    if doing_step is asserted, throw exog_action
%% abortStep :- thread_signal(indigolog_thread, (doing_step -> throw(exog_action) ; true)).
abortStep :- doing_step -> throw(exog_action); true.

%% -- mayEvolve(E1,H1,E2,H2,S)
%%    perform transition from (E1,H1) to (E2,H2) with result S
%%    trans = (E1,H1) performs a step to (E2,H2)
%%    final = (E1,H1) is a terminating configuration
%%    exog  = an exogenous actions occurred
%%    failed= (E1,H1) is a dead-end configuration

mayEvolve(E1, H1, E2, H2, S) :-
        catch( (
                 %% 1. assert flag doing_step
                 assert(doing_step),
                 %% 2. check if exog happens
                 (exists_pending_exog_event -> abortStep ; true),
                 %% 3. evolve
                 ( %% 3.1 first test if E1 H1 terminates
                   final(E1, H1)         -> S = final ;
                   %% 3.2 then test if E1 H1 has a good trans
                   trans(E1, H1, E2, H2) -> S = trans;
                   %% 3.3 report fail
                                            S = failed ),
                 %% 4. retract flag
                 retractall(doing_step)
               ),
               %% catch exception
               exog_action,
               %% exception handling
               (retractall(doing_step), S = exog)).


indigolog(E) :- initialize, thread_create(indigo(E,[]), ID, [at_exit(plan_done(ID)), alias(indigolog_thread), detached(true)]). %,
%% indigolog(E) :- initialize, indigo(E, []).

indigo(E, H) :-
        %% 1. first get rid of the tail of H when necessary and update currently
        %%    H2 is the header part of H
        handle_rolling(H, H2), !,
        %% 2. handle pending exog. events
        %%    exog events are added to the front of H2
        handle_exog(H2, H3), !,
        prepare_for_step,               %% turn off gc
        %% 3. compute next configuration E4, H4, S
        mayEvolve(E, H3, E4, H4, S), !, 
        wrap_up_step,                   %% turn on gc again
        (
          %% 4.1 clean up the history or execute the action
          S=trans  -> indigo2(H3, E4, H4);
          %% 4.2 program ends
          S=final  -> report_message(program, 'Success.');
          %% 4.3 exog event happens during trans, harly true
          %%     exog event is included in H3
          S=exog   -> (report_message(program, 'Restarting step.'), indigo(E, H3)); 
          S=failed -> report_message(program, 'Program fails.')
        ).

%% -- indigo2(+H1, +E, +H2)
%%    called from indigo/2 only after a successful TRANS
%%    H1 is the history BEFORE the transition
%%    E is the program that remains to execute
%%    H2 is the history AFTER the transition
indigo2(H, E, H) :- indigo(E, H). %% The case of TRANS for tests ?(P)
indigo2(H, E, [wait|H]) :-
        !,
        pause_or_roll(H, H1),
        doWaitForExog(H1, H2),
        indigo(E, H2).
indigo2(H,E,[stop_interrupts|H]) :- !, 
	indigo(E,[stop_interrupts|H]).
indigo2(H, E, [A|H]) :-
        indixeq(A, H, H1), %% execute domain action
        indigo(E, H1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exogenous events
%% Exogenous actions are stored in the local predicate indi_exog(Act)
%% until they are ready to be incorporated into the history
:- dynamic
        indi_exog/1.

%% check if exists any pending exogenous event
exists_pending_exog_event :- indi_exog(_).

%% -- exog_action_occurred(+L)
%%    called to report the occurrence of a list L of 
%%    exogenus actions (called from env. manager)
%%    First we add each exogenous event to the clause indi_exog/1 and
%%    in the end, if we are performing an evolution step, we abort the step.
exog_action_occurred([]) :- doing_step -> abortStep ; true.
%% exog_action_occurred([]).
exog_action_occurred([ExoAction|LExoAction]) :-
        assert(indi_exog(ExoAction)),   
        report_message(exogaction, ['Exog. Action *',ExoAction,'* occurred']),
	exog_action_occurred(LExoAction).

%% -- handle_exog(+History, -History2)
%%    History2 is History1 with all pending exog actions placed at the front
handle_exog(H1, H2) :- 
        %% 1 - Collect SYSTEM exogenous actions (e.g., debug)
	findall(A, (indi_exog(A), type_action(A, system)), LSysExog),
        %% 2 - Collect NON-SYSTEM exogenous actions (e.g., domain actions)
        findall(A, (indi_exog(A), \+ type_action(A, system)), LNormal),	
        %% 3 - Append the lists to the current hitory (system list on front)
	append(LSysExog, LNormal, LTotal),
        append(LTotal, H1, H2), 
	update_now(H2),
	%% 4 - Remove all indi_exog/1 clauses
	retractall(indi_exog(_)).
handle_exog(H1, H1). %% No exogenous actions, keep same history

%% -- doWaitForExog(+History1, -History2)
%%    wait continously until an exogenous action occurrs
doWaitForExog(H1,H2):- 	
        report_message(system(2), 'Waiting for exogenous action to happen'), 
        repeat, 
        handle_exog(H1,H2),
        (H2=H1 -> fail ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History

:- dynamic
        now/1,
        temp/2, %% Temporal predicate used for rolling forward
        has_valc/3, %% store cached fluent (F, V, H)
        rollednow/1,
        currently/2.

:- multifile
        initially/2.
        

%% -- before(?History1, ?History2)
%%    success if History1 is a previous hsitory of History2
before(H1, H2) :- append(_, H1, H2).

%% -- pasthist(-History)
%%    History is a past situation w.r.t. the actual situation
pasthist(H) :- now(H2), before(H, H2).

%% -- update_now(+History)
%%    updates the current History
update_now(H) :-
        retract(now(_)) -> assert(now(H));
                           assert(now(H)). %% if now(_) is empty at the beginning

%% -- roll_parameters(L, M, N)
%%    define parameters for roll/2
%%    L: the history has to be longer than this, or dont bother
%%    M: if the history is longer than this, forced roll (M >= L)
%%    N: the length of the tail of the history to be dropped and saved in preserve, set N=0 to never roll forward
%% roll_parameters(1,1,0).  %% Never roll forward
%% roll_parameters(20,40,5). %% can roll after 20, must roll after 40, keep 5 actions

must_roll(H) :- roll_parameters(_,M,N), length(H,L1), L1 > M, N>0.
can_roll(H) :- roll_parameters(L,_,N), length(H,L1), L1 > L, N>0.

clean_cache :- retractall(has_valc(_,_,_)).

%% -- Update the cache information by stripping out the subhistory H
update_cache(Htail) :-
	retract(has_valc(F, V, OldHis)), %% OldHis is the old longer history
	append(Hhead, Htail, OldHis),           %% H1 is the new shorter history
	assert(has_valc(F, V, Hhead)),  
	fail.
update_cache(_).

% %% -- roll_action_fluent(+Action, +Fluent)
% %%    Fluent requires update wrt executed Action
% %%    at this point Fluent may still contain free var
% roll_action_fluent(A, F) :-
% 	has_val_reg(F, V, [A]), %% compute one possible value for F (now F is ground)
% 	(\+ temp(F, V) -> assert(temp(F, V)) ; true), %% if new value, put it in temp/2
% 	fail. %% with fail, will go directly to the next rule instead from the beginning.
% roll_action_fluent(_, F) :- %% now update currently/2 with the just computed temp/2
% 	temp(F, _),
% 	retractall(currently(F,_)),
%         %% F needs a full update, remove all currently/2
%         %% Next obtain all values stored in temp/2 for that specific ground F
% 	temp(F,V), %% Get a new possible value (maybe many, backtrack)
% 	assert(currently(F,V)), %% Set the new possible value in currently/2
% 	retract(temp(F,V)), %% Remove the new possible value from temp/2
% 	fail.
% roll_action_fluent(_, _).

%% -- roll_action(A)
%%    roll currently/2 database with respect to action A
% roll_action(e(A,V)) :-
%         senses(A,F),
%         retractall(currently(F, _)),
%         assert(currently(F, V)),
%         fail.
% roll_action(A) :-
%         causes_val(A, F, _, _),
% 	roll_action_fluent(A, F),
% 	fail.
roll_action(A) :- %% TODO: what kind of actions?
	sets_val(A, F, V, []),
        %%% TEST: ziyang
        report_message(system(5), ['Action', A, 'changed', F, 'to', V]), 
	(\+ temp(F, V) -> assert(temp(F, V)) ; true),
	fail.
roll_action(_) :- %% handle all the temp/2
	retract(temp(F,V)),
	retractall(currently(F,_)), %% There should be just one currently/2 for F!
	assert(currently(F,V)),
	fail.
roll_action(_).

show_cache :-
        has_valc(F, V, H),
        report_message(system(6), ['Cached fluent', F, 'had value', V, 'at', H]),
        fail.
show_cache.

%% -- preserve(H)
%%    rolls forward the initial database currently/2 from [] to H
preserve([]).
preserve([A|H]) :-
        preserve(H), %% handle tail first
        report_message(system(4), ['Rolling action', A]), 
        roll_action(A), show_cache.
        %% update_cache([A]). %% TODO: ?why update_cache here
%%    [go_school, eat, clean, get_up] is a good example

%% -- split(+N, History1, History2, History3)
%%    succeeds if append(History2,History3,History1) and length(Histroy2)==N
split(0,H,[],H) :- ! .
split(N,[A|H],[A|H1],H2) :- N > 0, N1 is N-1, split(N1,H,H1,H2).
%% ?- split(2, [a, b, c, d, e], [a, b], [c, d, e]).

show_currently :-
        currently(F, V),
        report_message(system(6), ['Fluent', F, 'had value', V]),
        fail.
show_currently.

%% -- roll_db(+History1,-History2)
%%    roll from History1 to History2
%%    History1 is the current history (H1 = H2 + H3)
%%    Histroy2 will be the new history
roll_db(H1,H2) :- 
	roll_parameters(_,_,N), 
	split(N,H1,H2,H3), %% H3 is the tail of H1 that is going to be dropped
        report_message(system(3), ['(DB) ', 'Progressing the following sub-history: ', H3]), 
	preserve(H3),
        report_message(system(3), ['(DB) ', 'Updating cache...']), 
	update_cache(H3), %% Update the cache information
        show_currently, 
        report_message(system(3), ['(DB) ', 'Subhistory completely rolled forward']).

%%  Rolling forward means advancing the predicate currently(-,-) and
%%  discarding the corresponding tail of the history.
%% -- roll(+History1, -History2)
roll(H1, H2) :-
        report_message(system(0),'Rolling down the river (progressing the database).......'), 
	roll_db(H1, H2), 
        report_message(system(0), 'done progressing the database!'), 
        report_message(system(3), ['New History: ', H2]), 
	update_now(H2),         %% Update the current history	
	append(H2,HDropped,H1),	%% Extract what was dropped from H1
	retract(rollednow(HO)), %% Update the rollednow/1 predicate to store all that has been rolled forward
	append(HDropped,HO,HN), %% rollednow(H): H is the full system history
	assert(rollednow(HN)).

%% -- handle_olling(+History1, -History2)
%%    mandatory rolling forward
handle_rolling(H1,H2) :- must_roll(H1), !, roll(H1, H2).
handle_rolling(H1,H1).

%% -- pause_or_roll(+Histroy1, -History2)
%%    optional rolling forward
pause_or_roll(H1,H2) :- can_roll(H1), !, roll(H1, H2).
pause_or_roll(H1,H1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Projector

%% -- domain(-Value, +Domain)
%%    assigns a user-defined domain to a variable. 
domain(V, D) :- is_list(D) -> member(V, D); apply(D,[V]).

rdomain(V, D) :-
        (is_list(D) ->
            L=D ;
            bagof(P,apply(D,[P]),L)),
        shuffle(L,L2), !, member(V, L2).


%% SPECIAL PROJECTOR CASES FOR SYSTEM-WIDE FLUENTS
holds(neg(interrupts_running),H)  :- !, \+ holds(interrupts_running,H).
holds(interrupts_running,H)       :- !, \+ (H=[stop_interrupts|_]).

%% -- holds(+Condition,+H)
%%    Condition holds in H (i.e., Condition is possibly true at H)

%% negation normal form transformation %% Loyd-Topor Transforamtion
%% Define these for performance??? 
holds(neg(or(P1,P2)),H)   :- !, holds(and(neg(P1),neg(P2)),H).
holds(neg(and(P1,P2)),H)  :- !, holds(or(neg(P1),neg(P2)),H). 
holds(neg(neg(P)),H)      :- !, holds(P,H). 		
holds(neg(all(V,D,P)),H)  :- !, holds(some(V,D,neg(P)),H). 
holds(neg(some(V,D,P)),H) :- !, holds(all(V,D,neg(P)),H).
holds(neg(P),H)           :- proc(P,P1), !, holds(neg(P1), H).

%% non-atomic formulas
holds(and(P1,P2),H)  	:- !, holds(P1,H), !, holds(P2,H).
holds(or(P1,P2),H)   	:- !, ((holds(P1,H),!) ; (holds(P2,H),!)).
holds(some(V,D,P),H)    :- !, domain(O,D), subv(V,O,P,P1), holds(P1,H). %% After introducing domain, we always have grounded Condition to check
%% success when you can find one O such that P1 does not hold
holds(some(V,P),H)	:- !, subv(V,_,P,P1), holds(P1,H).
holds(neg(some(V,P)),H)   :- !, \+ holds(some(V,P), H).

holds(all(V,D,P),H)     :- !, \+((domain(O,D), subv(V,O,P,P1), \+ holds(P1,H))).
holds(P,H)           	:- proc(P,P1), !, holds(P1,H), !. %% No backtracking?

%% Evaluation of ground atoms. Atoms are either equality (fluent) atoms or 
%% prolog predicates possibly mentioning ground fluents.
%% if it's a prolog predicate then use good-old subf ***
%% if it's a ground equality atom then optimize a bit ***
holds(neg(P),H):- !, subf(P,P1,H), !, \+ call(P1).
%% vanila
%% holds(neg(P), H) :- !, checkgr(P), \+ holds(P, H) %% negation by failure

%% This is a special optimized case when evaluating Fluent = Value so as to
%% feed has_val/3 with the exact match that can guide the search
holds(T1=T2,H) :- ground(T1), ground(T2), 
		  liftAtom(T1, NameT1, ArgT1, LiftT1),  
		  liftAtom(T2, NameT2, ArgT2, LiftT2),
		  ( %% if LiftT1 has the form of prim_fluent!!!
                    (prim_fluent(LiftT1), \+ prim_fluent(LiftT2), !, 
                        subf(ArgT1,ArgT1Eval,H), !, 
                        T1Eval =..[NameT1|ArgT1Eval],
                        has_val(T1Eval,T2,H))
                  ; %% else if LiftT2 has the form of prim_fluent!!!
                    (prim_fluent(LiftT2), \+ prim_fluent(LiftT1), !,
                        subf(ArgT2,ArgT2Eval,H), !, 
                        T2Eval =..[NameT2|ArgT2Eval],
                        has_val(T2Eval,T1,H))).

holds(P,H) :-
        !, subf(P,P1,H), %% no backtrack here, open Var remains open, like father(lzy, B),  will be called later
        call(P1).

%% vanila
%% holds(neg(P),H) :- !, \+ holds(P,H).   /* Negation by failure */
%% holds(P,H) :- \+ proc(P,P1), subf(P,P1,H), call(P1). %% if P contains fluent like temp>2

%% -- sets_val(+Act,+Fluent,+V,+H)
%%    Act causes Fluent to be V in history H
%%    1. if Act is a standard action with a successor state axiom causes_val
sets_val(Act,F,V,H) :-
        causes_val(Act,F,V,C), holds(C,H).
%%    3. if Act is a sensing action without settles axiom
sets_val(e(Act,V),F,V,_) :- senses(Act,F), !.
%%    4. Act sets F indirectly
sets_val(e(Act,V),F,V2,H) :-
        senses(Act,V,F,V2,P), holds(P,H).
%%    5. Fluent can explicitly set by e(F,V).
%%       -- assume(+F,+V,+H,-[e(F,V)|H]). 
sets_val(e(F,V),F,V,_) :- prim_fluent(F).

:- multifile
        cache/1.

%% -- has_val_reg(+Fluent,-Value,+H)
%%    holds if Fluent has Value at H (proven by regression)
has_val_reg(F,V,[]) :- currently(F,V), !. %% if H is empty
has_val_reg(F, V, [Act|H]) :-
        %% else if Act changes F
        sets_val(Act, F, _, H), !,  %% only check Act and F first for performance
        sets_val(Act, F, V, H). 
has_val_reg(F, V, [_|H]) :- has_val(F,V,H). %% A do not change the fluent, check the tail of the history

%% -- has_val(+Fluent,?V,+H)
%%    calculate V for Fluent with H
has_val(F,V,H)  :-
        %% if F is cached type and there is no cache store
        %% compute all the cached values
        cache(F), \+ has_valc(F,_,H), 
        has_val_reg(F,V,H),
        assert(has_valc(F,V,H)), !.
has_val(F,V,H)  :-
        %% if F is cached type and there is cache info, bind those values
        cache(F), !, has_valc(F,V,H).
has_val(F,V,H)  :-
        %% F is not cached type, use normal regression
        has_val_reg(F,V,H). %% F is a fluent with NO cache

%% -- subf(+Condition1, ?Condition2, +H)
%%    Condition2 is Condition1 with all fluents replaced by a possible value at H
%%    !!!!! try to best to call subf with grounded P1, unless it is a rule
subf(P1,P2,_)  :- (var(P1) ; number(P1)), !, P2 = P1.
subf(P1,P2,H)  :- atom(P1), !, subf2(P1,P2,H). %% not compound like condition or simple fluent
subf(P1,P2,H)  :- P1=..[F|L1], subfl(L1,L2,H), P3=..[F|L2], subf2(P3,P2,H).

%% real substitution happens here
%% when P3 has open var, subf2 will give multiple sol
subf2(P3,P2,H) :-
        (ground(P3) ->
            prim_fluent(P3), !, has_val(P3, P2, H); %% grounded prim_fluent case, no need to back track 
            prim_fluent(P3), has_val(P3, P2, H)). %% ungrounded prim_fluent case, need backtrack to find the binding that has_val holds, like holds(some(n, light(n)=off), []).
subf2(P2,P2,_) :- \+ prim_fluent(P2).

subfl([],[],_). %% substitution for fluent list
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculation/Transition/Trans-Final

trans(interrupt(Trigger,Body),H,E1,H1) :-
    trans(while(interrupts_running,if(Trigger,Body,?(neg(true)))),H,E1,H1).

trans(interrupt(V,Trigger,Body),H,E1,H1) :- 
    trans(while(interrupts_running, 
    		pi(V,if(Trigger,Body,?(neg(true))))),H,E1,H1).  

final(interrupt(Trigger,Body),H) :-
    final(while(interrupts_running,if(Trigger,Body,?(neg(true)))), H).

final(interrupt(V,Trigger,Body),H) :- 
    final(while(interrupts_running, pi(V,if(Trigger,Body,?(neg(true))))),H).

% Note these fluents (e.g., halt) and special actions (e.g., halt_exec) need to be defined already.
trans(prioritized_interrupts(L),H,E1,H1) :- 
    expand_interrupts(L,E), !,
    trans(E,H,E1,H1).

trans(prioritized_interrupts_simple(L),H,E1,H1) :- 
        expand_interrupts(L,E), !,
        trans(E,H,E1,H1).

expand_interrupts([],stop_interrupts).

expand_interrupts([X|L],pconc(X,E)) :-
    expand_interrupts(L,E).

final(stop_interrupts,_) :- fail, !.
trans(stop_interrupts,H,[],[stop_interrupts|H]).

%% TRADITIONAL SEARCH (From [De Giacomo & Levesque 99])
%% Linear plans, ignore sensing
%% Search with a message
final(search(E,_),H) :- final(search(E),H).

trans(search(E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(search(E),H,E1,H1) ->
            report_message(program, 'Finished thinking: Plan found!') 
        ;
            report_message(program, 'Finished thinking: No plan found!'), 
            fail).

%% Basic search/1
%% search on E, using caching and replanning only when situation
%% is not the expected one
%% if findpath/3 wants to abort everything it has to throw exception search
final(search(E),H) :- final(E,H).

trans(search(E),H,followpath(E1,L),H1) :- 
        catch((trans(E,H,E1,H1), findpath(E1,H1,L)), search, fail).

%% -- findpath(+E,+H,-L)
%%    find a solution L for E at H; 
%%    L is the list [E1,H1,E2,H2,...,EN,HN] encoding
%%    each step evolution (Ei,Hi) where final(EN,HN)
%%    If last action was commit, then commit to the sub-plan found.
findpath(E,[commit|H],L) :- !, 
	(findpath(E,H,L) -> true ; throw(search)).
findpath(E,H,[E,H]) :- final(E,H).
findpath(E,H,[E,H|L]) :- 
	trans(E,H,E1,H1), 
	findpath(E1,H1,L).

%% -- followpath(E,L)
%%    execute program E wrt expected sequence of
%%    configurations L=[E,HEx,E1,H1,...]
%%    if the current history does not match the next expected one
%%    in L (i.e., H\=HEx), then redo the search for E from H
final(followpath(E,[E,H]),H) :- !.
final(followpath(E,_),H) :- final(E,H).  %% off path; check again????

trans(followpath(E,[E,H,E1,H1|L]),H,followpath(E1,[E1,H1|L]),H1) :- !.
trans(followpath(E,_),H,E1,H1) :- trans(search(E),H,E1,H1). %% redo search

%% -- query(+P, -S)
%%    call P and return true or false
trans(query(P, S), H, [], H) :- call(P) -> S=true; S=failed.

%% trans(par(E1, E2), H, [], H1) :- trans(E1, H, [], )

%% Wait and commit are two "meta" actions.
%% wait action tells the interpreter to wait until an exogenous action arrives
%% commit is used in search and searchc to commit to the plan computed so far
trans(wait,H,[],[wait|H])    :- !. %% wait is a no-op but encourages rolling db
trans(commit,S,[],[commit|S]).	   %% commit to the plan found so far! 
trans(abort,S,[],[abort|S]).	   %% completely abort execution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAIN CONSTRUCT

%% -- ??(P)
%%    a test action like ?(P) but it leaves a test(P) mark in H
trans(??(P),H,[],[test(P)|H]) :- holds(P,H). 
trans(?(P),H,[],H)            :- holds(P,H).

%% -- wndet(E1,E2)
%%    Weak nondeterministic choice of program
%%    try to execute program E1 first. If impossible, then try program E2 instead
final(wndet(E1,E2),H)      :- final(E1,H), final(E2,H). %% If E1 is not final, then E1 can 'trans'
trans(wndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) -> true ; trans(E2,H,E,H1).

%% -- ndet(E1,E2)
%%    nondeterministic choice of a program that trans
%%    Note1: if final config exist, no trans will happen
%%    Note2: better only use in search contruct unless the programs are primitive
final(ndet(E1,E2),H)          :- final(E1,H); final(E2,H). 
trans(ndet(E1,E2),H,E,H1)     :- 
        random(1,10,R), %% flip a coin!
	(R>5 ->
            (trans(E1,H,E,H1); trans(E2,H,E,H1));
            (trans(E2,H,E,H1); trans(E1,H,E,H1))).

%% -- rconc(E1,E2)
%%    random concurrency on 2 programs
final(rconc(E1,E2),H) :- final(E1,H), final(E2,H).
trans(rconc(E1,E2),H,rconc(E11,E22),H1) :- 
        ( random(1, 3, 1) -> 	% flip a coin!
            ((trans(E1,H,E11,H1), E22=E2); (trans(E2,H,E22,H1), E11=E1));
            ((trans(E2,H,E22,H1), E11=E1); (trans(E1,H,E11,H1), E22=E2)) ).

%% -- rconc(L)
%%    random concurrency on a list of programs L
final(rconc([]),_).
final(rconc([E|L]),H) :- final(E,H), final(rconc(L),H).

trans(rconc(L),H,rconc([E1|LRest]),H1) :-
	length(L,LL),
	random(0,LL, R),
	nth0(R,L,E),
	trans(E,H,E1,H1),
	select(E,L,LRest).	 

%% -- iconc(E)
%%    iterative concurrent execution of E, star + conc
final(iconc(_),_).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

%% -- conc(E1,E2)
%%    concurrent (interleaved) execution of E1 and E2
final(conc(E1,E2),H)               :- final(E1,H), final(E2,H).
trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).

%% -- pconc(E1,E2)
%%    prioritized conc. execution of E1 and E2 (E1>E2)
final(pconc(E1,E2),H)                :- final(E1,H), final(E2,H).
trans(pconc(E1,E2),H,pconc(E,E2),H1) :- trans(E1,H,E,H1). %% 
trans(pconc(E1,E2),H,E,H1) :- trans(bpconc(E1,E2,H),H,E,H1).

%% -- bpconc(E1,E2,H)
%%    used to improve the performance of pconc(_,_)
%%    does not reconsider process E1 as long as the history
%%    remains being H (at H, E1 is already known to be blocked)
trans(bpconc(E1,E2,H),H,E,H1) :-
        !, %% blocked history H
        trans(E2,H,E3,H1), 
        (H1=H -> E=bpconc(E1,E3,H) ; E=pconc(E1,E3)).
trans(bpconc(E1,E2,_),H,E,H1) :-
        %% for the other H
        trans(pconc(E1,E2),H,E,H1).

%% -- if(P,E1,E2)
%%    if control constuct
%%    Note: P can be an unground rule to bind some params
final(if(P,E1,E2),H) :- ground(P), !, (holds(P,H) -> final(E1,H) ; final(E2,H)).
final(if(P,E1,_),H)  :- holds(P,H), final(E1,H). %% P may get gound here?
final(if(P,_,E2),H)  :- holds(neg(P),H), final(E2,H).
trans(if(P,E1,E2),H,E,H1)     :-
        ground(P), !, (holds(P,H) -> trans(E1,H,E,H1) ;  trans(E2,H,E,H1)).
trans(if(P,E1,E2),H,E,H1)     :-
        !, ((holds(P,H), trans(E1,H,E,H1)) ; (holds(neg(P),H), trans(E2,H,E,H1))).

%% -- while(P,E)
%%    while construct
%%    Note: terminate directly when P does not hold
final(while(P,E),H)                    :- holds(neg(P),H) ; final(E,H).
trans(while(P,E),H,[E1,while(P,E)],H1) :- holds(P,H), trans(E,H,E1,H1).

%% -- for(V, +Domain, +P)
%%   perform program P(V) with all elements in Domain
final(for(V,D,P),H)     :-
        D\=[], atom(D), !,
        bagof(X, domain(X,D), L), 
        final(for(V,L,P),H).
final(for(V,[F|L],P),H) :-
        subv(V,F,P,P1), final(P1,H),
        final(for(V,L,P),H).
final(for(_,[],_),_).

trans(for(V,D,P),H,E1,H1)                  :-
        D\=[], atom(D), !, %% D is not a list
        bagof(X, domain(X,D), L), 
        trans(for(V,L,P),H,E1,H1).
trans(for(V,[F|L],P),H,[E1,for(V,L,P)],H1) :- 
	subv(V,F,P,P1), trans(P1,H,E1,H1).
trans(for(_,[],_),H,[],H). %% TODO: is this necessary?

%% -- rpi(-V,+Domain,+E)
%%    real nondeterministic choice of argument from Domain
final(rpi((V,D),E),H)       :- !, final(rpi(V,D,E),H).
final(rpi(V,D,E),H)         :- rdomain(W,D), subv(V,W,E,E2), !, final(E2,H).
trans(rpi((V,D),E),H,E1,H1) :- !, trans(rpi(V,D,E),H,E1,H1).
trans(rpi(V,D,E),H,E1,H1)   :- rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).

%% -- pi(-V,+Domain,+E)
%%    Note: trans will not check if the binding leads to final success
%%          only make sure the binding leads to one successful trans
final(pi([V|L],E),H) :- !, final(pi(L,pi(V,E)),H).
final(pi(V,E),H)     :- !, subv(V,_,E,E2), !, final(E2,H).
final(pi([],E),H)    :- !, final(E,H).
final(pi((V,D),E),H) :- !, final(pi(V,D,E),H).
final(pi(V,D,E),H)   :- domain(W,D), subv(V,W,E,E2), !, final(E2,H).

trans(pi([V|L],E),H,E1,H1) :- !, trans(pi(L,pi(V,E)),H,E1,H1).
trans(pi(V,E),H,E1,H1)     :- subv(V,_,E,E2), !, trans(E2,H,E1,H1).
trans(pi([],E),H,E1,H1)    :- !, trans(E,H,E1,H1).
trans(pi((V,D),E),H,E1,H1) :- !, trans(pi(V,D,E),H,E1,H1).
trans(pi(V,D,E),H,E1,H1)   :- !, domain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).

%% -- star(E)/star(E,N)
%%    run program E 0 - inf/N times
final(star(_),_).
final(star(_, _), _).
trans(star(E,1),H,E1,H1)               :- !, trans(E,H,E1,H1).
trans(star(E,N),H,[E1,star(E,M)],H1)   :- N>1, trans(E,H,E1,H1), M is N-1.
trans(star(E),H,[E1,star(E)],H1)       :- trans(E,H,E1,H1).


%% LAST FINAL
final([E|L],H)       :- final(E,H), final(L,H).
final(E,H)           :- proc(E,E2), !, final(E2,H).
final([],_).

trans([E|L],H,[E1|L],H2)  :- trans(E,H,E1,H2).
trans([E|L],H,E1,H2)      :- \+ L=[], final(E,H), trans(L,H,E1,H2).

%% LAST TRANS FOR PROCEDURES AND PRIMITIVE ACTIONS (everything else failed)
%% Replace the arguments(fluents) by their value, check if it is a primitive action
%% and finally check for preconditions.

%% -- in_pmAct_form(A)
%%    check if A has "the form" of a primitive action
%%    A can be a primitive action with unevaluated fluents as arguments
%%    Note: the same holds for prim_fluent, fluents can have another fluent as args
%%    like: isGold(locRobot)
in_pmAct_form(A):-
        liftAtom(A, _, _, LiftedA), 
        prim_action(LiftedA), !. %% Note: LiftedA gets a grounding here
%% -- calc_act_arg(P, P1, H)
%%    compute the arguments of an action
calc_act_arg(P,P1,H):-
        atomic(P) -> P1=P;
        (P =..[Function|LArg], subfl(LArg,LArg2,H), 
            P1=..[Function|LArg2]).

trans(E,H,E1,H1)     :- proc(E,E2), !, trans(E2,H,E1,H1).
trans(E,H,[],[E1|H]) :-
        %% Note: the [] will be added to the head of the remaining procedure
        in_pmAct_form(E),
	calc_act_arg(E,E1,H), %% Hope E1 gets all ground here
	prim_action(E1), !,   %% Note: E1 gets a grounding here
	poss(E1,P), 
	holds(P,H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Execution
%% -- indixeq(H1,H2,H3)
%%    short for indigolog execution?? Implementation of execution of an action.
%%    H1 is the original history, H2 is H1 with the new action to be executed
%%    and H3 is the resulting history after executing such new action.

% % (1) - No action was performed so we dont execute anything
% indixeq(H,H,H).
% % (2) - The action is not a sensing one: execute and ignore its sensing
% indixeq(H,[Act|H],[Act|H]) :- \+ senses(Act,_), execute(Act,_), assertz_action(Act).
% % (3) - The action is a sensing one for fluent F: execute sensing action
% indixeq(H,[Act|H],[e(F,Sr),Act|H]) :- senses(Act,F), execute(Act,Sr), assertz_action(Act).

%% -- execute(Act, Sr) :- ask_execute(Act, Sr).
%% -- ask_execute(Act,Sr) :-
%%         update_count(M),         
%%         write('Step '), write(M), write(' is '), write(Act), write('.'), 
%%         senses(Act, _)
%%         % senses(Act, Fluent)
%% 	->
%%         (nl,
%%      %%   (write('Queried value for '), write(Fluent), write(' is:'), Query=..[Fluent, Sr], Sr=call(Query))
%% 	write('sensing input for '), write(Act), write(':'), read_action(Sr))
%% 	;
%% 	nl.

:- dynamic
        indigolog_action/1,
        excuting_action/1, 
        excuting_action/4, %% assert the current action so that Python can query
        action_counter/1.  %% just a counter


refresh_counter :- retractall(action_counter(_)), assertz(action_counter(0)).

update_counter(M) :-
        retract(action_counter(N)),
        M is N+1, assert(action_counter(M)).

%% -- type_action(+Action, -Type)
%%    finds out the type of an action
type_action(Act, sensing) :- senses(Act); senses(Act, _); senses(Act, _, _, _, _), !.
type_action(Act, system) :- system_action(Act), !.
type_action(_, nonsensing). %% for the rest

:- dynamic 
    executing_action/1,
    indigolog_action/1.

%% -- assert to do action to the database
assert_action(Act) :-
        retractall(indigolog_action(_)),
        retractall(executing_action(_)),
        assertz(executing_action(Act)),
        assertz(indigolog_action(Act)).

%% -- 
execute_action(Action, H, Type, Outcome) :-
        %% Increment action counter by 1 and store action information
	update_counter(M), 
	assert(executing_action(M, Action, H, Type)), %% Store new action to execute
        assert_action(Action), %% maybe duplicate
        thread_get_message(got_sensing(Action, Outcome)),
	retract(executing_action(M, _, _, _)),
        retract(executing_action(_)),
	report_message(system(2), 
		['Action *', (M, Action), '* completed with outcome:', Outcome]).

%% -- handle_sensing(+Action, +[Action|History], +Value, -NewHistory)
%%    change the NewHistory to encode the sensing result of Action
%%    1. old way with senses/2
handle_sensing(Act, [Act|H], Sr, [e(F, Sr), Act|H]) :- senses(Act,F).
% %%    2. new way with senses/1, effects are defined by settles and rejects
% handle_sensing(Act, [Act|H], Sr, [e(Act, Sr), Act|H]) :- senses(Act).
% %%    3. not used senses/5
% handle_sensing(Act, [Act|H], Sr, [e(F, Sr), Act|H]) :- senses(Act,Sr,F,_,_). %% only add the sensing result to the history, no need to validate condition

indixeq(Act, H, H2) :- %% execution of system actions -- just add it to history
        type_action(Act, system), !, %% e(_,_) are system actions!!!
        H2 = [Act|H],
        update_now(H2).
indixeq(Act, H, H2) :- %% execution of sensing actions
        type_action(Act, sensing), !,
        report_message(system(1), ['Sending sensing Action *', Act, '* for execution.']),
        execute_action(Act, H, sensing, S), !, %% do we really need to execute or just check the database??? 
        (S=failed ->
            report_message(error, ['Action *', Act, '* FAILED to execute at history: ', H]),
            H2 = [abort, failed(Act)|H], %% request abortion of program, next time transfinal will receive abort!!!
            update_now(H2)
        ;
            report_message(action, ['Action *', Act, '* EXECUTED SUCCESSFULLY with sensing outcome: ', S]),
            wait_if_neccessary,
            handle_sensing(Act, [Act|H], S, H2), %% add sensing outcome!
            update_now(H2)).
indixeq(Act, H, H2) :- %% execution of nonsensing actions
        type_action(Act, nonsensing), !,
        report_message(system(1), ['Sending nonsensing action *', Act, '* for execution.']),
        execute_action(Act, H, nonsensing, S), !, 
        (S=failed ->
            report_message(error, ['Action *', Act, '* could not be executed at history: ', H]),
            H2 = [abort, failed(Act)|H],
            update_now(H2)
        ;
            report_message(action, ['Action *', Act, '* COMPLETED SUCCESSFULLY.']),
            wait_if_neccessary,
            H2 = [Act|H],
            update_now(H2)).
