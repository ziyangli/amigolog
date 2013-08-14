:- dynamic action/1.
:- dynamic get_action/2.
:- dynamic get_action_input/3.


% get_action_input just looks how many inputs are given in the action and will put them in a list Action_input. 
% This way easier to handle within Python
get_action_input(Action_with_inputs, Action, Action_input) :-
        Action_with_inputs =.. [Action|Action_input].

get_action(Action,Action_input) :-
	executing_action([Action_with_inputs]),
	get_action_input(Action_with_inputs,Action,Action_input).

%% Outcome 'failed' repeats action for now, otherwise success is assumed.
assert_done(Outcome) :-
%%    	Action_with_inputs =.. [Action|Action_input],
 	executing_action([Action_with_inputs]),
	thread_send_message(indigolog_thread, got_sensing(Action_with_inputs, Outcome)). %% wait until get sensing Result

assert_done(Outcome,Action,Action_input) :-
	Action_with_inputs =.. [Action|Action_input],
	thread_send_message(indigolog_thread, got_sensing(Action_with_inputs, Outcome)). %% wait until get sensing Result
