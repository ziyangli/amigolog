%% extended projector

%% -- kwhether(+Fluent,+H)
%%    Fluent is known true or false in H
%%    Assumes that after sensing F, F may change but it will remain known
%%    We may probably want to add some "forgeting" mechanism.. either by a 
%%      state condition or special actions
holds(kwhether(F),[])     :- !, (currently(F,_); initially(F,_)).
holds(kwhether(F),[Act|_]):- (senses(Act,F) ; Act=e(F,_)), !.
holds(kwhether(F),[_|H])  :- holds(kwhether(F),H).

%% -- know(Fluent)
%%    Fluent evaluates to something
holds(know(F),H)     :- !, holds(F=_,H).



holds(neg(impl(P1,P2)),H) :- !, holds(and(P1,neg(P2)),H).  
holds(neg(equiv(P1,P2)),H):- !, holds(or(and(P1,neg(P2)),and(neg(P1),P2)),H).

%% implication as a macro
holds(impl(P1,P2),H)  	:- !, holds(or(neg(P1),P2),H).
holds(equiv(P1,P2),H) 	:- !, holds(and(impl(P1,P2),impl(P2,P1)),H).

%% special projector cases for system-wide fluents
isTrue(interrupts_running,H) :- !, \+ (H=[stop_interrupts|_]).
isTrue(neg(interrupts_running),H) :- !, \+ isTrue(interrupts_running,H).
isTrue(haveExecuted(Act),H) :- !, member(Act,H). %% true if the Act has been executed
isTrue(neg(haveExecuted(Act)),H) :- !, \+ isTrue(haveExecuted(Act),H).

%% general projector
isTrue(C,H):- kholds(C,H). %% TODO: kholds

%%    2. if Act is an sensing action with settles axiom
sets_val(e(Act,Sr),F,V,[Act|H]) :- settles(Act,Sr,F,V,C), kholds(C,H). %% will it backtrack when you have multiple settles and the C does not holds?


%% -- kholds(+Condition/Rule,+H)
%%    Only used after settles or rejects!!!
%%    Condition is known to be true at H
%%    or Use Rule to get some value
kholds(P,H) :-
        ground(P) ->  
        \+ holds(neg(P),H) %% ?TODO: why neg? put in neg so matching has better performance???
        ;
        ( warn(['kholds/2 called with open variables: ',P]), 
          holds(P,H), %% first get P grounded? It seems here holds/2 will try to find a grouded solution!!!
          \+ holds(neg(P),H) ).


has_val_reg(F, V, [e(A,Sr), A|H]) :-
        %% else if the previous Act change F and the current Act as a sensing one does not reject the V
        !, has_val(F,V,H), \+ (rejects(A,Sr,F,V,C), kholds(C,H)).

roll_action(e(A,S)) :- 
	settles(A, S, F, V, C), %% A may settle F	
	prim_fluent(F),				
	kholds(C, []), %% Value of F is settled to V, TODO: why [] here? Because roll_action is the final step, and when it is called, the tail history is rolled to be []??
	retractall(currently(F, _)),		
	assert(currently(F, V)), %% Update value of F to unique value V
	fail.
roll_action(e(A,S)) :- %% A may reject F
	rejects(A, S, F, V, C),			
	prim_fluent(F),				
	currently(F,V), %% choose a potential value V for rejection
	kholds(C, []),  %% V should be rejected!
	retractall(currently(F, V)), %% then, retract V from F
	fail.


%% :- dynamic
%%         forget/2, 
%%         forget/3. %% Act makes F unknown

%% -- forget(Act, H, F)
%%    So far, one forgets the value of F when it is sensed (may be improved)
%%    TODO: so when you define senses(Act, F), also need to define forget(Act, F)
%% forget(Act, _, F) :- forget(Act, F).

%% has_val_reg(F,V,[A|H]) :-
%%         %% else if Act does not forget F, check the previous V, and make sure A
%%         %% does not set V at this step, replace the last has_val_reg/3 when introduce
%%         %% forget/3.
%%         \+ forget(A,H,F), has_val(F,V,H), \+ sets_val(A,F,_, H).



%% -- set(+Fluent)/unset(+Fluent)
%% Special high-level actions to set and unset Fluent
prim_action(set(_)).
prim_action(unset(_)).
poss(set(F), ground(F)).
poss(unset(F), ground(F)).
has_val(F,V,[set(F)|_])  :- !, V=true.
has_val(F,V,[unset(F)|_]):- !, V=false.


%% is_an_action(+Act)
%% Check if Act has the form of a primitive action
is_an_action(A) :-
        %% when A is unground or atomic
        \+ var(A), %% in case is_an_action(X).
        %% double negation uninstantiated A
        \+ \+ (prim_action(A) ; exog_action(A)), !. 

is_an_action(A) :-
        %% when A is grounded compound with stranger args
        compound(A),
        liftAtom(A, _, _, LA),
        (prim_action(LA); exog_action(LA)), !.
