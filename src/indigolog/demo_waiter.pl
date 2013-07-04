%% Interface to the outside world via read and write
execute(A,Sr) :- ask_execute(A,Sr).
%% exog_occurs(A) :- ask_exog_occurs(A).
% exog_occurs(_) :- fail.

table(N) :- N=0; N=1; N=2; N=3; N=4.
arms(N) :- N=left; N=right.    % left and right
side(Side) :- Side=left; Side=right.

%% test action
prim_action(show_error) :- write('Mission impossible!!!'), nl.

prim_fluent(avail_arm(N)) :- arms(N).
initially(avail_arm(_), yes).

prim_action(go_table(N)) :- table(N).
poss(go_table(N), at_table(N)=no).
causes_val(go_table(N), at_table(N), yes, true).
causes_val(go_table(N), at_table(M), no, and(table(M), neg(M=N))).

prim_fluent(at_table(N)) :- table(N).
initially(at_table(N), no) :- table(N), \+ N=0.
initially(at_table(0), yes).

%% number of drink to serve at table(N)
prim_fluent(n_of_d_to_serve(table(N))) :- table(N), \+ N=0.
cache(n_of_d_to_serve(_)).
% initially(n_of_d_to_serve(table(N)), 0) :- table(N), \+ N=0.
initially(n_of_d_to_serve(table(1)), 2).
initially(n_of_d_to_serve(table(2)), 3).
initially(n_of_d_to_serve(table(3)), 4).
initially(n_of_d_to_serve(table(4)), 0).

prim_fluent(total_request).
initially(total_request, 9).
% initially(total_request, 0).

prim_fluent(drink_in_hand).
initially(drink_in_hand, 0).

exog_action(request(table(N), _)) :- table(N), \+ N=0.
causes_val(request(table(N), Num), n_of_d_to_serve(table(N)), NewNum, NewNum = Num + n_of_d_to_serve(table(N))).
causes_val(request(table(_), Num), total_request, NewNum, NewNum = total_request + Num).

prim_action(putdown(Arm, Loc)) :- arms(Arm), table(Loc), \+ Loc=0.
poss(putdown(Arm, _), avail_arm(Arm)=no).
causes_val(putdown(Arm, _), avail_arm(Arm), yes, true).
causes_val(putdown(_, _), total_request, NewNum, NewNum is total_request-1).
causes_val(putdown(_, Loc), n_of_d_to_serve(table(Loc)), NewNum, NewNum is n_of_d_to_serve(table(Loc))-1).
causes_val(putdown(_, _), drink_in_hand, NewNum, NewNum is drink_in_hand-1).

proc(hasFreeArm, some(n, avail_arm(n)=yes)).
proc(hasDrink, some(n, avail_arm(n)=no)).
proc(hasRequest, total_request>0).

prim_action(grab(N)) :- arms(N).
poss(grab(N), avail_arm(N)=yes).
causes_val(grab(N), avail_arm(N), no, true).
causes_val(grab(_), drink_in_hand, NewNum, NewNum is drink_in_hand+1).

%% grab one or two bottles of drink
proc(fill_gripper,  while( and(hasFreeArm, total_request>drink_in_hand), pi(x, grab(x)))).

proc(off_load(Loc), while( and(n_of_d_to_serve(table(Loc))>0, hasDrink), pi(n, putdown(n, Loc)))).

proc(next_table_to_serve(Loc),  n_of_d_to_serve(table(Loc))>0).

proc(serve_tables, while(hasDrink, pi(n,
            [ ?(next_table_to_serve(n)), go_table(n), off_load(n) ]))).
%% proc(serve_tables, while(hasDrink, [ ?(next_table_to_serve(N)), go_table(N), off_load(N) ])).

proc(waiter_program, [while(hasRequest, [fill_gripper, serve_tables, go_table(0)])]).

proc(detest,  [conc([?(hasDrink), fill_gripper, serve_tables], fill_gripper)]).
%% proc(ddtest,  [?(neg(true)), show_error]).
%% proc(saytest, [say('hey'), say('ziyang li!')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% replace
% repl(input_list, Output_list, Index, value)
repl([_|L], [X|L], 1, X).
repl([Y|L1], [Y|L2], N, X) :- repl(L1, L2, M, X), N is M+1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%            end of waiter_program             %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
