
%% -- Trans(E,+History1,E1,-History2)
%%    one execution step of program E from History1 leads to program E1 with History2
%% -- Final(+E,+History)
%%    Program E at History is final

/* CONGOLOG CONSTRUCTS                                               */
/*    iconc(E)    : iterative concurrent execution of E              */
/*    conc(E1,E2) : concurrent (interleaved) execution of E1 and E2  */
/*    pconc(E1,E2): prioritized conc. execution of E1 and E2 (E1>E2) */
/*    bpconc(E1,E2,H): used to improve the performance of pconc(_,_) */

final(iconc(_),_).
final(conc(E1,E2),H)  :- final(E1,H), final(E2,H).
final(pconc(E1,E2),H) :- final(E1,H), final(E2,H).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).
trans(pconc(E1,E2),H,E,H1) :-    % bpconc(E1,E2,H) is for when E1 blocked at H
    trans(E1,H,E3,H1) -> E=pconc(E3,E2) ; trans(bpconc(E1,E2,H),H,E,H1).

%% -- bpconc(E1,E2,H) does not reconsider process E1 as long as the history
%%    remains being H (at H, E1 is already known to be blocked)
trans(bpconc(E1,E2,H),H,E,H1) :- !,
    trans(E2,H,E3,H1),  % blocked history H
    (H1=H -> E=bpconc(E1,E3,H) ; E=pconc(E1,E3)).
trans(bpconc(E1,E2,_),H,E,H1) :- trans(pconc(E1,E2),H,E,H1).

/* INTERRUPTS */
trans(interrupt(Trigger,Body),H,E1,H1) :-
    trans(while(interrupts_running,if(Trigger,Body,?(neg(true)))),H,E1,H1).

trans(interrupt(V,Trigger,Body),H,E1,H1) :- 
    trans(while(interrupts_running, 
    		pi(V,if(Trigger,Body,?(neg(true))))),H,E1,H1).  

final(interrupt(Trigger,Body),H) :-
    final(while(interrupts_running,if(Trigger,Body,?(neg(true)))), H).

final(interrupt(V,Trigger,Body),H) :- 
    final(while(interrupts_running, pi(V,if(Trigger,Body,?(neg(true))))),H).

%% Note these fluents (e.g., halt) and special actions (e.g., halt_exec) need to be defined already.
trans(prioritized_interrupts(L),H,E1,H1) :- 
    expand_interrupts([interrupt(haveExecuted(halt),  halt_exec),
    		       interrupt(haveExecuted(abort), abort_exec),
    		       interrupt(haveExecuted(pause), break_exec),
    		       interrupt(haveExecuted(reset), reset_exec),
		       interrupt(haveExecuted(debug), debug_exec)|L],E), !,
    trans(E,H,E1,H1).

trans(prioritized_interrupts_simple(L),H,E1,H1) :- 
%%    expand_interrupts([interrupt(haveExecuted(halt),halt_exec)|L],E), !,
        expand_interrupts(L,E), !,
        trans(E,H,E1,H1).

expand_interrupts([],stop_interrupts).

expand_interrupts([X|L],pconc(X,E)) :-
        expand_interrupts(L,E).

%% trans and final for system actions (e.g., show_debug, halt_exec, etc.)    
trans(stop_interrupts,H,[],[stop_interrupts|H]).
final(stop_interrupts,_) :- fail, !.

/*  GOLOG CONSTRUCTS                                          */
/*  These include primitive action, test, while, if           */
/*  nondeterministic choice and nondeterministic iteration.   */

final([],_).
final(star(_),_).
final(star(_,_),_).
final([E|L],H)       :- final(E,H), final(L,H).
final(ndet(E1,E2),H) :- final(E1,H) ; final(E2,H).
final(if(P,E1,E2),H) :- ground(P), !, (holds(P,H) -> final(E1,H) ; final(E2,H)).
final(if(P,E1,_),H)  :- holds(P,H), final(E1,H).
final(if(P,_,E2),H)  :- holds(neg(P),H), final(E2,H).
final(while(P,E),H)  :- holds(neg(P),H) ; final(E,H).


trans(pi([],E),H,E1,H1)    :- !, trans(E,H,E1,H1).
trans(pi([V|L],E),H,E1,H1) :- !, trans(pi(L,pi(V,E)),H,E1,H1).
trans(pi((V,D),E),H,E1,H1) :- !, trans(pi(V,D,E),H,E1,H1).
trans(pi(r(V),D,E),H,E1,H1):- !, rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,D,E),H,E1,H1)   :- !, domain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,E),H,E1,H1)     :- subv(V,_,E,E2), !, trans(E2,H,E1,H1).

final(pi([],E),H)    :- !, final(E,H).
final(pi([V|L],E),H) :- !, final(pi(L,pi(V,E)),H).
final(pi(V,E),H)     :- !, subv(V,_,E,E2), !, final(E2,H).
final(pi((V,D),E),H) :- !, final(pi(V,D,E),H).
final(pi(V,D,E),H)   :- domain(W,D), subv(V,W,E,E2), !, final(E2,H).


final(E,H)           :- proc(E,E2), !, final(E2,H).

trans([E|L],H,E1,H2)      :- \+ L=[], final(E,H), trans(L,H,E1,H2).
trans([E|L],H,[E1|L],H2)  :- trans(E,H,E1,H2).
trans(?(P),H,[],H)        :- holds(P,H).
trans(ndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(if(P,E1,E2),H,E,H1) :- ground(P), !,
	(holds(P,H) -> trans(E1,H,E,H1) ;  trans(E2,H,E,H1)).
trans(if(P,E1,E2),H,E,H1)  :- !,
	((holds(P,H), trans(E1,H,E,H1)) ; (holds(neg(P),H), trans(E2,H,E,H1))).
trans(star(E,1),H,E1,H1)  :- !, trans(E,H,E1,H1).
trans(star(E,N),H,[E1,star(E,M)],H1)   :- N>1, trans(E,H,E1,H1), M is N-1.
trans(star(E),H,E1,H1)       :- trans(E,H,E1,H1).
trans(star(E),H,[E1,star(E)],H1)       :- trans(E,H,E1,H1).
trans(while(P,E),H,[E1,while(P,E)],H1) :- holds(P,H), trans(E,H,E1,H1).

%% LAST TRANS FOR PROCEDURES AND PRIMITIVE ACTIONS (everything else failed)
%% Replace the arguments by their value, check that it is a primitive action
%% and finally check for preconditions.
trans(E,H,E1,H1)    :- proc(E,E2), !, trans(E2,H,E1,H1).
trans(A,H,[],[A|H]) :- system_action(A), !.
final(A,_) 	    :- system_action(A), !, fail.

trans(E,H,[],[E1|H])    :- 
	calc_arg(E,E1,H),
	prim_action(E1), 
	poss(E1,P), 
	holds(P,H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -- rrobin(L)
%%    round-robin concurrency
trans(rrobin(L),H,rrobin(L2),H1) :-
	select(E,L,LRest),
	trans(E,H,E1,H1),
	append(LRest,[E1],L2).
final(rrobin(L),H) :- final(rconc(L),H).

%% Execute E atomically (i.e., as a transaction)
trans(atomic(E),H,[],[atomic(E2)|H2]) :- trans(E,H,E2,H2).
final(atomic(E),H) :- final(E,H).

%% -- gexec(P,E)
%%    guarded execution of program E wrt condition P
%%    execute program E as long as condition P holds, finish E if neg(P) holds
final(gexec(_,E), H) :- final(E,H).
trans(gexec(P,E), H, gexec2(P,E1), H1) :- %% P needs to be a simple fluent
        assume(P, true, H, H2),           %% Set P to be TRUE
        trans(E, H2, E1, H1).
final(gexec2(P,E), H) :- holds(neg(P),H); final(E,H).
trans(gexec2(P,E), H, gexec2(P,E1), H1) :- holds(P,H), trans(E,H,E1,H1).

%% -- goal(PSucc,E,PFail,ERec)
%%    full guarded execution
%%    PSucc: finalize successfully if PSucc holds
%%    E: the program to be executed
%%    PFail: Terminate the program E and execute recovery procedure ERec
final(goal(PSucc,E,_,_), H) :- holds(PSucc,H); final(E,H).
trans(goal(PSucc,_,PFail,ERec), H, E2, H2) :-
	holds(neg(PSucc),H),
	holds(PFail,H),
	trans(ERec,H, E2, H2).
trans(goal(PSucc,E,PFail,ERec), H, goal(PSucc,E2,PFail,ERec), H2) :-
	holds(neg(PSucc),H),
	holds(neg(PFail),H),
	trans(E,H,E2,H2).

%% -- abort process identified with P by setting P to false in H
trans(abort(P), H, [], H1) :- assume(P, false, H, H1).

%% -- simulation of exogenous actions E
trans(sim(E),H,[],[sim(E)|H]):- !, calc_arg(E,E1,H), exog_action(E1).

%% Time bounded steps
%% time(E,Sec)  : make the *first* step in E before Sec seconds
%% ttime(E,Sec) : make every step before in E before Sec seconds
%% TODO: timeout/3?
trans(time(E,Sec),H,E2,H2) :- timeout(trans(E,H,E2,H2), Sec, fail).
final(time(E,Sec),H) :-	timeout(final(E,H), Sec, fail).
trans(ttime(E,Sec),H,time(E2,Sec),H2) :- trans(time(E,Sec),H,E2,H2).
final(ttime(E,Sec),H) :- final(time(E,Sec),H).

%% Perform a transition on E, aborting though if an exogenous action happens
%% meanwhile and Cond holds in H
%% requires exog_interruptable/3 from main cycle
final(exogint(E,_Cond),H) :- final(E,H).
trans(exogint(E,Cond),H,exogint(E2,Cond),H2) :- 
	exog_interruptable(trans(E,H,E2,H2), holds(Cond,H), Status),
	(Status=ok -> 
            true;
            report_message('TF', system(3),'Computation of trans/4 aborted due to exog events'),
		E2=E, H2=H
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% vanila TRANS FINAL
       /* (a) - CONGOLOG */

final(iconc(_),_).
final(conc(E1,E2),H) :- final(E1,H), final(E2,H).
final(pconc(E1,E2),H) :- final(E1,H), final(E2,H).


trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).
trans(pconc(E1,E2),H,E,H1) :-
	trans(E1,H,E3,H1)
	->
	E=pconc(E3,E2)
	;
	(trans(E2,H,E3,H1), E=pconc(E1,E3)).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

       /* (b) - GOLOG */
final([],_).
final([E|L],H) :- final(E,H), final(L,H).
final(ndet(E1,E2),H) :- final(E1,H) ; final(E2,H).
final(if(P,E1,E2),H) :-
	holds(P,H)
	->
        final(E1,H)
        ;
        final(E2,H).
final(star(_),_).
final(while(P,E),H) :- \+ holds(P,H) ; final(E,H).
final(pi(V,E),H) :- subv(V,_,E,E2), final(E2,H).
final(E,H) :- proc(E,E2), final(E2,H).

trans([E|L],H,[E1|L],H2) :- trans(E,H,E1,H2). % neglect the rest procedure, check if trans(E, H, E1, H2) holds, trans([a1, p1], H, [E1|p1], H2)
trans([E|L],H,E1,H2) :- \+ L=[], final(E,H), trans(L,H,E1,H2). % only situation: if E is star(_)? ensure star(_) is executed at least one time?
trans(?(P),H,[],H) :- holds(P,H).
trans(ndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(if(P,E1,E2),H,E,H1) :-
        holds(P,H)
	->
        trans(E1,H,E,H1)
        ;
        trans(E2,H,E,H1).
trans(star(E),H,[E1,star(E)],H1) :- trans(E,H,E1,H1).
trans(while(P,E),H,[E1,while(P,E)],H1) :- holds(P,H), trans(E,H,E1,H1).
trans(pi(V,E),H,E1,H1) :- subv(V,_,E,E2), trans(E2,H,E1,H1).
trans(E,H,E1,H1) :- proc(E,E2), trans(E2,H,E1,H1). % if E is a procedure
trans(E,H,[],[E|H]) :- prim_action(E), poss(E,P), holds(P,H). % if E is a prim_action

       /* (c) -  SEARCH (ignoring exogenous or other concurrent actions) */
/* If (E,H) is a final state then finish. Otherwise, look for a straight
   path (E1,L) without looking at exogenous actions */
final(search(E),H) :- final(E,H).
trans(search(E),H,followpath(E1,L),H1) :- trans(E,H,E1,H1), findpath(E1,H1,L).

/* Look for a good path without looking at exogenous actions */
findpath(E,H,[E,H]) :- final(E,H).
findpath(E,H,[E,H|L]) :- trans(E,H,E1,H1), findpath(E1,H1,L).

/* When we have a followpath(E,L), try to advance using the list L
   in an offline manner.
   If it is not possible to advance any more redo the search to continue */
final(followpath(E,[E,H]),H) :- !.
final(followpath(E,_),H) :- final(E,H).  /* off path; check again */
trans(followpath(E,[E,H,E1,H1|L]),H,followpath(E1,[E1,H1|L]),H1) :- !.
trans(followpath(E,_),H,E1,H1) :- trans(search(E),H,E1,H1).  /* redo search */

       /* (d) -  INTERRUPTS */
prim_action(start_interrupts).
prim_action(stop_interrupts).
prim_fluent(interrupts).
causes_val(start_interrupts, interrupts, running, true).
causes_val(stop_interrupts, interrupts, stopped, true).
poss(start_interrupts, true).
poss(stop_interrupts,  true).

proc(interrupt(V,Trigger,Body),            /* version with variable */
    while(interrupts=running, pi(V,if(Trigger,Body,?(neg(true)))))).
proc(interrupt(Trigger,Body),              /* version without variable */
    while(interrupts=running, if(Trigger,Body,?(neg(true))))).
proc(prioritized_interrupts(L),[start_interrupts,E]) :- expand_interrupts(L,E).
expand_interrupts([],stop_interrupts).
expand_interrupts([X|L],pconc(X,E)) :- expand_interrupts(L,E).

trans(prioritized_interrupts(L),H,E1,H1) :- 
        expand_interrupts([interrupt(haveExecuted(halt),  halt_exec),
                           interrupt(haveExecuted(abort), abort_exec),
                           interrupt(haveExecuted(pause), break_exec),
                           interrupt(haveExecuted(reset), reset_exec),
                           interrupt(haveExecuted(debug), debug_exec)|L],E), !,
    trans(E,H,E1,H1).

holds(neg(haveExecuted(A)),S)	:- !, \+ holds(haveExecuted(A),S).
holds(haveExecuted(A),S) 	:- !, member(A,S). %% true if the A has been executed


% final(interrupt(Trigger,Body),H)   :-
%         final(if(Trigger,Body,?(neg(true))), H).
% final(interrupt(V,Trigger,Body),H) :- 
%         final(pi(V,if(Trigger,Body,?(neg(true)))), H).
% trans(interrupt(Trigger,Body),H,E1,H1)   :-
%         trans(if(Trigger,Body,?(neg(true))),H,E1,H1).
% trans(interrupt(V,Trigger,Body),H,E1,H1) :- 
%         trans(pi(V,if(Trigger,Body,?(neg(true)))),H,E1,H1).

% expand_interrupts([X|L],pconc(X,E)) :- L\=[], expand_interrupts(L,E), !.
% expand_interrupts([X|L],X) :- L=[].

% trans(prioritized_interrupts(L),H,E1,H1) :- 
%         expand_interrupts(L,E), !,
%         trans(E,H,E1,H1).

