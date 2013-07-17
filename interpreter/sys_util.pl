%% killing a thread means signal it with an "abort" event
thread_kill(ThreadId) :-
        thread_signal(ThreadId, throw(abort)),
        wait(ThreadId, _). %% TODO: where is wait defined?
thread_wait(ThreadId, Status) :- 
        current_thread(ThreadId, _) -> thread_join(ThreadId, Status) ; true.

%% Turn on/off the automatic garbage collector
turn_on_gc  :- set_prolog_flag(gc, true).
turn_off_gc :- set_prolog_flag(gc, false).

%% Set string construct to be ` to the calling module
:- module_transparent set_backquoted_string/0.
set_backquoted_string :- set_prolog_flag(backquoted_string, true). 
:- module_transparent reset_backquoted_string/0.
reset_backquoted_string :- set_prolog_flag(backquoted_string, false). 

%% Perform a call catching it if there is an exception
%% If so, print message and then either fail or succeed
%% -- report_message/2, from string_util
catch_fail(Call, Message) :-
	catch(Call,E,
              (report_message(warning,[Message, ' ---> ', E]), fail)).
catch_succ(Call, Message) :-
	catch(Call,E,
              (report_message(warning,[Message, ' ---> ', E]), true)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message tools

:- dynamic 
	debug_level/1,
	warn_off/0.

%% -- set_debug_level(+N)
%%    set the debug level to N, only messages system(M) with M>N are shown

%%    set on/off for warning messages
set_debug_level(warn_off) :- (warn_off -> true ; assert(warn_off)), !.
set_debug_level(warn_on) :- retractall(warn_off), !.

%%    ! to avoid redo set_debug_level(N)

%% set te debug level to be below N (the higher the N, the more debug messages)
set_debug_level(N) :-
       (integer(N) ->
	retractall(debug_level(_)),
        assert(debug_level(N)),
        report_message(system(0), ['Debug level set to',N]) , !
        ;
        report_message(system(0), ['Input an interger.']), fail).

%% -- report_message(+Type, +Message)       
%%    report Messsage of Type
report_message(T, L) :- 
	is_list(L), !, 
	maplist(any_to_string,L,LS),
	any_to_string(' ', Space),
	join_string(LS, Space, M2), %% Include space between each element
	report_message(T, M2).

report_message(system(N), _) :- %% High level messages will be ignored
        debug_level(N2), N>N2, !.
report_message(system(N), T) :- !, 
        N2 is N-1,
        tab(N2), %% the higher level, the larger tab
        write('DEBUG '),  write(N), write(': '), writeln(T).

report_message(warning, T) :- !,
	(warn_off -> true ; write('!!! WARNING: '), writeln(T)).

report_message(error, T) :- !,
        write('!!! ERROR ----> '),  writeln(T).

report_message(program, T) :- !,
        write('  ***** PROGRAM:: '),  writeln(T).

report_message(action, T) :- !,
        write('>>>>>>>>>>>> ACTION EVENT:: '),  writeln(T).

report_message(sensing, T) :- !,
        write('--------------> SENSING EVENT:: '),  writeln(T).

report_message(exogaction, T) :- !,
	nl,
        write('=========> EXOGENOUS EVENT:: '), writeln(T).

report_message(user, T) :- !,
        write('  **** USER MESSAGE:: '),  writeln(T).

report_message(_, T) :-
        write('  **** OTHER EVENT:: '),  writeln(T).

error(M) :-
        report_message(error, M),
        report_message(error, 'Execution will be aborted!'), abort.

warn(M) :- report_message(warning, M).

