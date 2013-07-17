%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : indigolog.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
        doing_step/0, %% flag to show a step is being CALCULATED

        indi_exog/1, %% store exog events not managed yet
                
        excuting_action/1, 
        excuting_action/4, %% assert the current action so that Python can query
        action_counter/1,  %% counter to show the current step

        preempt/1,

        currently/2, 
        has_valc/3, %% store cached fluent (F, V, H)
        
        now/1,       %% store actual history
        rollednow/1, %% part of th actual history that has been rolled
        temp/2.      %% temporal predicate used for rolling forward
        
:- multifile
        exog_action/1,
        
        senses/1,
        senses/5, 
        senses/2,

        rescues/2, %% proc. P resuces Action
        
        initially/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(data_util).
:- ensure_loaded(sys_util).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TOP LEVEL MAIN CYCLE
%% -- indigolog(E)
%%    E is a high-level program
%%    the history H is a list of actions (prim or exog), initially [] (empty)
%%    sensing reports are inserted as actions of the form e(fluent,value)

indigolog(E) :-
        %% 1. clean up indigolog database
        initialize,
        %% 2. run indigo/2 in the indigolog_thread
        thread_create(indigo(E,[]),ID,[at_exit(program_ends(ID)),alias(indigolog_thread),detached(true)]).

indigo(E,H) :-
        %% 1. update currently/2 and has_valc/3 to speed up the regression
        handle_rolling(H,HRolled), !,
        prepare_for_step, %% turn off gc
        %% 2. compute next configuration EEvolved, HEvolved, S
        mayEvolve(E,HRolled,EEvolved,HEvolved,S), !, 
        wrap_up_step, %% turn on gc again
        (
          %% 3.1 clean up the history or execute the action
          S=trans  -> indigo2(HRolled,EEvolved,HEvolved);
          %% 3.2 program ends
          S=final  -> report_message(program,'Success.');
          %% 3.3 exog events occur during trans, harly happens
          S=exog   ->
          (report_message(program,'Restarting step.'), handle_exog(HRolled,H2), indigo(E,H2));
          %% 3.4 program fails
          S=failed -> report_message(program,'Program fails.')
        ).

%% -- indigo2(+HbT,+EaT,+HaT)
%%    called from indigo/2 only after a successful TRANS
%%    HbT is the history BEFORE TRANS
%%    EaT is the program that remains to execute
%%    HaT is the history AFTER TRANS
%% 1. TRANS does not add action to History, ?(P)/query(Q)/etc
indigo2(H,E,H)                   :- indigo(E,H).
%% 2. INTERRUPTION stops
indigo2(H,E,[stop_interrupts|H]) :- !, indigo(E,[stop_interrupts|H]).
%% 3. execute domain action, A can be single Act or par(A1, A2)
indigo2(H,E,[A|H])               :- 
        indixeq(A,H,H2),
        update_now(H2),
        (preempt(A) ->
            retract(preempt(A)), rescues(A,Erec), indigo([Erec|E],H2)
        ;
            indigo(E,H2)).
        
%% -- mayEvolve(+EbE,+HbE,-EaE,-HaE,S)
%%    perform transition from (EbE,HbE) to (EaE,HaE) with result S
mayEvolve(E1,H1,E2,H2,S) :-
        catch( ( %% 1. assert flag doing_step
                 assert(doing_step),
                 %% 2. check if exog happens
                 (exists_pending_exog_event -> abortStep ; true),
                 %% 3. evolve
                 ( %% 3.1 (E1,H1) is a terminating configuration
                     final(E1,H1)       -> S = final ;
                   %% 3.2 (E1,H1) performs a step to (E2,H2)
                     trans(E1,H1,E2,H2) -> S = trans;
                   %% 3.3 (E1,H1) is a dead-end configuration
                     S = failed ),
                 %% 4. retract flag
                 retractall(doing_step) ),
               %% catch exception
               exog_action,
               %% an exogenous actions occurred
               (retractall(doing_step), S = exog) ).

program_ends(ID) :- write('Exit '), write(ID), writeln('!').

%% -- abortStep
%%    if is calculating a step, throw exog_action
abortStep :- doing_step -> throw(exog_action); true.

%% -- predicate to prepare everything for computing the next single step.
%%    disable gc to speed up
prepare_for_step :- turn_off_gc. %% before computing a step
wrap_up_step :-                  %% after computing a step
        turn_on_gc,
        garbage_collect.

initialize :-
        retractall(currently(_,_)),
        forall(initially(F,V),assert(currently(F,V))),
        clean_cache, %% clean has_valc/2
        reset_indigolog_dbs,
        refresh_counter.

reset_indigolog_dbs :-
        retractall(doing_step),
        retractall(indi_exog(_)),
        retractall(preempt(_)), 
        retractall(rollednow(_)),
        retractall(now(_)),
        update_now([]),
        assert(rollednow([])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Execution
%% -- indixeq(+Act,+HbE,-HaE)
%%    indigolog execution
%%    Act is the action to be executed
%%    HbE is the history before execution
%%    HaE is the history after execution

%% 1. execution of sensing actions
indixeq(Act,H,H2) :-
        sensing_action_p(Act), !, 
        execute_action(Act,H,sensing,S),
        (S=failed ->
            report_err(Act,H), H2 = [abort,failed(Act)|H]
        ;
            report_done(Act), handle_sensing(Act,[Act|H],S,H2)).
%% 2. execution of parallel actions
indixeq(par(A1,A2),H,H2) :-
        execute_action([A1,A2],H,parallel,S),
        (S=failed ->
            now(H1), report_err((A1,A2),H1), H2 = [abort,failed(A2,A1)|H1]
        ;
            now(H1), report_done((A1,A2),S), H2=[A2,A1|H1]).
%% 3. execution of domain actions
indixeq(Act,H,H2) :-
        execute_action(Act,H,domain,S),
        (S=exog ->
            assert(preempt(Act)), now(H2)
        ;
            (S=failed ->
                now(H1),report_err(Act,H1), H2 = [abort,failed(Act)|H1]
            ;
                now(H1), report_done(Act,S), H2 = [Act|H1])).

%% -- fails(+Act,+H)
%%    on_condition of Act does not hold in H
fails(Act,H) :-
        on_condition(Act,P),
        \+ holds(P,H).

%% -- sensing_actionP(+Act)
%%    test if Act is a sensing action
sensing_action_p(Act) :- senses(Act); senses(Act,_); senses(Act,_,_,_,_), !.

%% -- execute_action(+AL,+H,+Type,-Outcome)
%% 1. parallel actions
execute_action([A1,A2],H,parallel,Outcome) :-
        update_counter(N),
        report_xeq(parallel, (A1,A2)),
        assert_execution(N,[A1,A2],parallel,H),
        (thread_get_message(got_sensing(A2, Ob1)),
            thread_get_message(got_sensing(A1, Ob2))
        ;
            thread_get_message(got_sensing(A1, Ob1)),
            thread_get_message(got_sensing(A2, Ob2))), 
        clean_execution(N),
        Outcome = [Ob1,Ob2].
%% 2. other actions
execute_action(Act,H,Type,S) :-
        %% increment action counter by 1 and store action information
	update_counter(N), 
        report_xeq(Type,Act),
        assert_execution(N,Act,Type,H),
        repeat,
        thread_get_message(Message),
        (Message = exog(ExogAct) ->
            exog_action_occurred([ExogAct]),
            handle_exog, now(H1), 
            (fails(Act,H1) -> S=exog; fail)
        ;
            Message = got_sensing(Act,S)),
        clean_execution(N).

report_xeq(Type, Act) :-
        report_message(system(1), ['sending', Type, 'action *', Act, '* for execution.']).
report_err(Act, H) :-
        report_message(error, ['action *', Act, '* FAILED to execute at history: ', H]).
report_done(Act, S) :-
        report_message(action, ['action *', Act, '* EXECUTED with sensing outcome: ', S]).

refresh_counter :- retractall(action_counter(_)), assertz(action_counter(0)).

%% -- update_counter(-N)
%%    update the counter and output the current step
update_counter(N) :-
        retract(action_counter(M)),
        N is M+1, assert(action_counter(N)).

%% -- store new actions to execute
assert_execution(N,Act,Type,H) :-
	assert(executing_action(N,Act,Type,H)),
        assert(executing_action(Act)).

%% -- 
clean_execution(N) :-
        retract(executing_action(N,_,_,_)),
        retract(executing_action(_)).

%% -- handle_sensing(+Action, +[Action|History], +Value, -NewHistory)
%%    change the NewHistory to encode the sensing result of Action
%%    1. old way with senses/2
handle_sensing(Act, [Act|H], Sr, [e(F, Sr), Act|H]) :- senses(Act,F).
% %%    2. new way with senses/1, effects are defined by settles and rejects
% handle_sensing(Act, [Act|H], Sr, [e(Act, Sr), Act|H]) :- senses(Act).
% %%    3. not used senses/5
% handle_sensing(Act, [Act|H], Sr, [e(F, Sr), Act|H]) :- senses(Act,Sr,F,_,_). %% only add the sensing result to the history, no need to validate condition

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exogenous events
%% Exogenous actions are stored in the local predicate indi_exog/1
%% until they are ready to be incorporated into the history

%% check if exists any pending exogenous event
exists_pending_exog_event :- indi_exog(_), !.

%% -- exog_action_occurred(+L)
%%    called to report the occurrence of a list L of
%%    exogenus actions (called from Python executive)
%%    First we add each exogenous event to the clause indi_exog/1 and
%%    in the end, if we are performing an evolution step, we abort the step.
exog_action_occurred([]) :- (doing_step -> abortStep ; true), !.
exog_action_occurred([ExoAction|LExoAction]) :-
        assert(indi_exog(ExoAction)),   
        report_message(exogaction, ['Exog. Action *',ExoAction,'* occurred']),
	exog_action_occurred(LExoAction).

%% -- handle_exog(+History1, -History2)
%%    History2 is History1 with all pending exog actions placed at the front
handle_exog(H1,H2) :- 
        findall(A,indi_exog(A),ExogL),
        append(ExogL,H1,H2), 
	update_now(H2),
	retractall(indi_exog(_)), !.
handle_exog(H1,H1) :- !. %% No exogenous actions, keep same history

handle_exog :-
        now(H1),
        retract(indi_exog(Exog)),
        append(Exog,H1,H2),
        update_now(H2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% History

%% -- handle_olling(+History1, -History2)
%%    mandatory rolling forward
handle_rolling(H1,H2) :- must_roll(H1), !, roll(H1, H2).
handle_rolling(H1,H1).

%% -- pause_or_roll(+Histroy1, -History2)
%%    optional rolling forward
pause_or_roll(H1,H2) :- can_roll(H1), !, roll(H1, H2).
pause_or_roll(H1,H1).

%% -- roll_parameters(L, M, N)
%%    define parameters for roll/2
%%    L: the history has to be longer than this, or dont bother
%%    M: if the history is longer than this, forced roll (M >= L)
%%    N: the length of the tail of the history to be dropped and saved in preserve/1, set N=0 to never roll forward

%% roll_parameters(1,1,0).   %% Never roll forward
%% roll_parameters(20,40,5). %% can roll after 20, must roll after 40, keep 5 actions

must_roll(H) :- roll_parameters(_,M,N), length(H,L1), L1 > M, N>0.
can_roll(H)  :- roll_parameters(L,_,N), length(H,L1), L1 > L, N>0.

%%  Rolling forward means advancing the predicate currently(-,-) and
%%  discarding the corresponding tail of the history.
%% -- roll(+History1, -History2)
roll(H1, H2) :-
        report_message(system(0),'Progressing the database.......'), 
	roll_db(H1, H2), 
        report_message(system(0), 'Done progressing the database!'),
        report_message(system(3), ['New History is:', H2]),
	update_now(H2),         %% Update the current history	
	append(H2,HDropped,H1),	%% Extract what was dropped from H1
	retract(rollednow(HO)), %% Update the rollednow/1 predicate to store all that has been rolled forward
	append(HDropped,HO,HN),
	assert(rollednow(HN)).

%% -- roll_db(+History1,-History2)
%%    roll from History1 to History2
%%    History1 is the current history (H1 = H2 + H3)
%%    Histroy2 will be the new history
roll_db(H1,H2) :- 
	roll_parameters(_,_,N), 
	split(N,H1,H2,H3), %% H3 is the tail of H1 that is going to be dropped
        report_message(system(3), ['(DB)', 'Progressing the following sub-history:', H3]), 
	preserve(H3),
        report_message(system(3), ['(DB)', 'Updating cache...']), 
	update_cache(H3),
        %% show_currently, 
        report_message(system(3), ['(DB)', 'Subhistory completely rolled forward']).

%% -- preserve(H)
%%    rolls forward the initial database currently/2 from [] to H
preserve([]).
preserve([A|H]) :-
        preserve(H), %% handle tail first
        report_message(system(4), ['Rolling action', A]), 
        roll_action(A).

roll_action(A) :-
        causes_val(A, F, V, P), holds(P, []), %% Note: V is grounded by evaluating P
        %% clean the cache info generated during evaluating P
        clean_cache([]),                      
        report_message(system(6), ['Action', A, 'changed', F, 'to', V]),
        assert(temp(F, V)),
        fail.
roll_action(e(A, V)) :-
        senses(A, F),
        report_message(system(6), ['Sensing action', A, 'changed', F, 'to', V]),
        retractall(currently(F, _)), %% in fact, sensed fluents usually do not have initial value
        assert(currently(F, V)),
        fail.   
roll_action(_) :- %% handle all the temp/2
	retract(temp(F,V)),
	retractall(currently(F,_)),
	assert(currently(F,V)),
	fail.
roll_action(_).

%% -- update_cache(+Htail)
%%    update the cache information by stripping out the subhistory Htail
update_cache(Htail) :-
	retract(has_valc(F, V, OldHis)),
	append(Hhead, Htail, OldHis),
	assert(has_valc(F, V, Hhead)),  
	fail.
update_cache(_).

clean_cache    :- retractall(has_valc(_,_,_)).
clean_cache(H) :- retractall(has_valc(_, _, H)).

%% -- update_now(+History)
%%    updates the current History
update_now(H) :-
        retractall(now(_)),
        assert(now(H)).

%% -- split(+N, History1, History2, History3)
%%    succeeds if append(History2,History3,History1) and length(Histroy2)==N
split(0,H,[],H) :- ! .
split(N,[A|H],[A|H1],H2) :- N > 0, N1 is N-1, split(N1,H,H1,H2).
%% ?- split(2, [a, b, c, d, e], [a, b], [c, d, e]).

%% -- before(?History1, ?History2)
%%    success if History1 is a previous hsitory of History2
before(H1, H2) :- append(_, H1, H2).

%% -- pasthist(-History)
%%    History is a past situation w.r.t. the actual situation
pasthist(H) :- now(H2), before(H, H2).

show_currently :-
        currently(F, V),
        report_message(system(6), ['Fluent', F, 'had value', V]),
        fail.
show_currently.

show_cache :-
        has_valc(F, V, H),
        report_message(system(6), ['Cached fluent', F, 'had value', V, 'at', H]),
        fail.
show_cache.

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

%% negation normal form transformation
%% Loyd-Topor Transforamtion

holds(neg(or(P1,P2)),H)   :- !, holds(and(neg(P1),neg(P2)),H).
holds(neg(and(P1,P2)),H)  :- !, holds(or(neg(P1),neg(P2)),H). 
holds(neg(neg(P)),H)      :- !, holds(P,H). 		
holds(neg(all(V,D,P)),H)  :- !, holds(some(V,D,neg(P)),H). 
holds(neg(some(V,D,P)),H) :- !, holds(all(V,D,neg(P)),H).
holds(neg(P),H)           :- proc(P,P1), !, holds(neg(P1), H).

%% non-atomic formulas
holds(and(P1,P2),H)  	:- !, holds(P1,H), !, holds(P2,H).
holds(or(P1,P2),H)   	:- !, ((holds(P1,H),!) ; (holds(P2,H),!)).
holds(some(V,D,P),H)    :- !, domain(O,D), subv(V,O,P,P1), holds(P1,H).
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

%% -- par(A1,A2)
%%    parallel execution of two primitive actions
%%    Note: only use carefully with primitive actions. Otherwise is buggy.
final(par(A1,A2),H)               :- final(A1,H), final(A2,H).
trans(par(A1,A2),H,Eo,[par(A1,A2)|H]) :- trans(A1,H,[],H1), trans(A2,H1,Eo,_).

%% -- rconc(E1,E2)
%%    random concurrency on 2 programs
final(rconc(E1,E2),H) :- final(E1,H), final(E2,H).
trans(rconc(E1,E2),H,rconc(E11,E22),H1) :- 
        ( random(1, 3, 1) -> %% flip a coin!
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

%% 1. normal trans
trans([E|L],H,[E1|L],H2)  :- trans(E,H,E1,H2).
%% 2. when the head is empty (resulted from previous success trans)
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

%% Mapping
:- dynamic
        settles/5,
        rejects/5.

%%    causes_val like predicates, define effects of sensing actions
%% -- rejects(+SensingAct, +SensedResult, +Fluent, +Value, +Condition)
%%    If Condition holds and SensingAct gets SensedResult, then Fluent does not
%%    get Value
%%    reject(smell, 0, locWumpus, Y, adj(locRobot, Y)) means
%%    if smell gets 0 as result, then ALL the adjcent location Y
%%    of locRobot can not be location of the Wumpus.
%% -- settles(+SensingAct, +SensedResult, +Fluent, +Value, +Condition)
%%    settles(senseGold, 1, isGold(L), true, L=locRobot).
