:- dynamic indigolog_action/1.
:- dynamic indigolog_plan/1.

:- dynamic action/1.
:- dynamic get_action/2.
:- dynamic get_action_input/3.

:- dynamic rm_exec_action_from_plan/1.

:- dynamic executing_action_duplicated/1.
:- dynamic action/1.
:- dynamic action_input/1.
:- dynamic executing_action/1.

:- multifile 
		indigolog_action/1,
                executing_action/1.

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!!!!!!!!!!!!!!!! COMMENT WHEN CONNECTION WITH INDIGOLOG IS USED !!!!!!!!!!!!!!!!!!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% indigolog_action(Action_with_inputs) :-
%% 	indigolog_plan([Action_with_inputs|Tail]).

%% rm_exec_action_from_plan(Y) :-
%% 	indigolog_plan([_|Tail]),
%% 	retract(indigolog_plan(X)),
%% 	assert(indigolog_plan(Tail)).
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!! COMMENT UNTIL THIS LINE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

retract_facts_for_get_action(X) :-
	retract(action(_)),
	retract(action_input(_)),
	retract(executing_action_duplicated(_)).

get_action(Action,Action_input) :-
    % First duplicate action plan so that I can assert / delete something out of the same list to check concurrent actions.    
	duplicate_action(X),
	(get_action_list(Action2,Action_input2);
	action(Action),
	action_input(Action_input)).

duplicate_action(X) :-
	executing_action(Action_with_inputs),
	assert(executing_action_duplicated(Action_with_inputs)).

get_action_list(Action,Action_input) :-
	check_concurrent_action(Action_with_inputs),
	get_action_input(Action_with_inputs,New_Action,New_Action_input),

	((action(Old_Action),
	append(Old_Action, [New_Action], Appended_Action),
	retract(action(_)),
	assert(action(Appended_Action)));
	(not(action(_)),
	assert(action([New_Action])))),

	((action_input(Old_Action_input),
	append(Old_Action_input, [New_Action_input], Appended_Action_Input),
	retract(action_input(_)),
	assert(action_input(Appended_Action_Input)));	
	(not(action_input(_)),
	assert(action_input([New_Action_input])))),

	get_action_list(Action2,Action_input2).	


check_concurrent_action(Action_with_inputs) :-
	executing_action_duplicated([Action_with_inputs|Tail]),
	retract(executing_action_duplicated(_)),
	assert(executing_action_duplicated(Tail)).


% get_action_input just looks how many inputs are given in the action and will put them in a list Action_input. 
% This way easier to handle within Python
get_action_input(Action_with_inputs, Action, Action_input) :-
        Action_with_inputs =.. [Action|Action_input].

get_length_list(List,Length) :-
	length(List,Length).

%% Outcome 'failed' repeats action for now, otherwise success is assumed.
assert_done(Outcome,Action,Action_input) :-
	Action_with_inputs =.. [Action|Action_input],
	thread_send_message(indigolog_thread, got_sensing(Action_with_inputs, Outcome)). %% wait until get sensing Result






/*
executing_action([
	navigate_generic(lookat_point_3d, 1.19, -1.0, 0.8), % = X-POI,Y-POI,Z-POI
	spindle(send_goal,30,0.8),							% 30 = waittime, 0.8 = Z-POI
	say('I found a drink'),
	head(reset),
	spindle(reset,20)
	]).
*/


%% get_action(Action,Action_input) :-                                                                                                          
%% 	% Be aware that if more than 8 inputs are used, change this in get_action_input predicate
%% 	indigolog_action(Action_with_inputs),
%% 	get_action_input(Action_with_inputs,Action,Action_input).


%% % Outcome 'failed' repeats action for now, otherwise success is assumed.
%% assert_done(Outcome) :-
%% 	indigolog_action(Action_with_inputs),
%% 	assert(got_sensing(Action_with_inputs, Outcome)). %% wait until get sensing Result

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Action query in swipl     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Plan with multiple actions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Plan examples:

%indigolog_plan([say('give me a drink'),gripper(left,open),say('this is it')]).
%indigolog_plan([say('haha what a joke'),navigate_generic(goal_pose_2d,2,0,0)]).
%indigolog_plan([say('do a step forward'), navigate_generic(goal_pose_2d,2.1,0,0), say('do a step backward'),navigate_generic(goal_pose_2d,0,0,3.13),say('yeah i can do it')]).

%indigolog_plan([navigate_generic(lookat_point_3d,left,1.19,-1,0.75), arm(to_point,left,1.19,-1,0.75),  gripper(left,open), arm(to_point2,left,1.19,-1,0.75), gripper(left,close), arm(lift,left), arm(retract,left)]). % = coke can
%indigolog_plan([navigate_generic(lookat_point_3d,left,1.19,-0.9,0.75), arm(to_point,left,1.19,-0.9,0.75),  gripper(left,open), arm(to_point2,left,1.19,-0.9,0.75), gripper(left,close), arm(lift,left), arm(retract,left)]). % = coke can
%indigolog_plan([navigate_generic(lookat_point_3d,left,1,0,0.76), arm(to_point,left,1,0,0.76), arm(to_point2,left,1,0,0.76), arm(lift,left), arm(retract,left)]). % = coke can

%%%%%% FINDING OBJECT %%%%%%

/*
indigolog_plan([
	navigate_generic(lookat_point_3d, 1.19, -1.0, 0.8), % = X-POI,Y-POI,Z-POI
	spindle(send_goal,30,0.8),							% 30 = waittime, 0.8 = Z-POI
	head(send_goal, 1.19, -1.0, 0.8), 					% = X-POI,Y-POI,Z-POI			
	perception_recognition(object, 2.5),				% = 2.5 seconds perception ON
	% Now a check in IndiGolog if object is found
	say('I found a drink'),
	head(reset),
	spindle(reset,20)]).
*/

%%%%%% GRASP OBJECT %%%%%%

%Grasp coke example (coke at X=1.22, Y=-1.1, Z = 0.8)

/*
indigolog_plan([navigate_generic(lookat_point_3d,1.22, -1.1, 0.8), %'front' (y-offset = 0) should be added, when possible
				%% PREPARE GRASP
				arm(prepare_grasp,left),
				spindle(send_goal,0,0.8), % Timeout, Z-value 
				%% PREPARE ORIENTATION
				navigate_generic(prepare_grasp_orientation,left,1.22, -1.1, 0.8), %'left'/'right' (y-offset = 0) should be added, when possible
				%% GRIPPER OPEN
				gripper(left,open),
				%% UPDATE OBJECT POSITION
				spindle(send_goal_laser,20,0.8), 
				head(send_goal, 1.22, -1.1, 0.8),
				%perception_recognition(laser_2d,2.0,1.22, -1.1, 0.8), %(2.0 = time input, x, y, z)  CRASHES IN SIMULATION. TEST ON ROBOT!!
				%% PREPARE PRE-GRASP POSITION
				head(reset),
				spindle(high,20),
				%% ARM TO PRE-GRASP POSITION
				arm(to_pre_grasp_point,left,1.22, -1.1, 0.8),    % uses pre_grasp = true (but will not get to final point yet..?
				%% GRASP
				arm(grasp,left,1.22, -1.1, 0.8),
				%% GRIPPER CLOSE
				gripper(left,close), 
				%% GRIPPER CLOSE
				arm(lift,left),
				%% GRIPPER CLOSE
				arm(retract,left),
				%% CARRYING POSITION
				arm(carrying,left)]).
*/

%(with coke from test file) 
/*
indigolog_plan([navigate_generic(lookat_point_3d,1, 2, 0.88), %'front' (y-offset = 0) should be added, when possible
				%% PREPARE GRASP
				arm(prepare_grasp,left),
				spindle(send_goal,1, 2, 0.88), % Timeout, Z-value 
				%% PREPARE ORIENTATION
				navigate_generic(prepare_grasp_orientation,left,1, 2, 0.88), %'left'/'right' (y-offset = 0) should be added, when possible
				
				head(send_goal, 1, 2, 0.88),
				perception_recognition(object,2.5),

				%% GRIPPER OPEN
				gripper(left,open),
				%% UPDATE OBJECT POSITION
				spindle(send_goal_laser,20,0.8), 
				head(send_goal, 1, 2, 0.88),
				%perception_recognition(laser_2d,2.0,1.22, -1.1, 0.8), %(2.0 = time input, x, y, z)  CRASHES IN SIMULATION. TEST ON ROBOT!!
				%% PREPARE PRE-GRASP POSITION
				head(reset),
				spindle(high,20),
				%% ARM TO PRE-GRASP POSITION
				arm(to_pre_grasp_point,left,1, 2, 0.88),    % uses pre_grasp = true (but will not get to final point yet..?
				%% GRASP
				arm(grasp,left,1, 2, 0.88),
				%% GRIPPER CLOSE
				gripper(left,close), 
				%% GRIPPER CLOSE
				arm(lift,left),
				%% GRIPPER CLOSE
				arm(retract,left),
				%% CARRYING POSITION
				arm(carrying,left)]).
*/

%%% Plan with single actions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Human interaction
%indigolog_plan([say('give me a drink')]).

%% Grippers
%indigolog_plan([gripper(left,open)]).
%indigolog_plan([gripper(left,close)]).
%indigolog_plan([gripper(right,open)]).
%indigolog_plan([gripper(right,close)]).
%indigolog_plan([gripper(both,open)]).
%indigolog_plan([gripper(both,close)]).

%% Arms
%indigolog_plan([arm(prepare_grasp,right)]).
%indigolog_plan([arm(to_pre_grasp_point,left,1,0,1.5)]).
%indigolog_plan([arm(grasp,left,1,0,1.5)]).
%indigolog_plan([arm(lift,right)]).
%indigolog_plan([arm(retract,left)]).
%indigolog_plan([arm(carrying,left)]).

%% Head
%indigolog_plan([head(reset)]).
%indigolog_plan([head(send_goal,4.5,2,0.5)]).

%% Spindle: spindle(action,waittime,inputs)
%indigolog_plan([spindle(reset,20)]).
%indigolog_plan([spindle(high,20)]).
%indigolog_plan([spindle(medium,20)]).
%indigolog_plan([spindle(low,20)]).
%indigolog_plan([spindle(send_goal,20,0.8)]).
%indigolog_plan([spindle(send_goal_laser,30,0.8)]).

%% Navigation
%indigolog_plan([navigate_generic(goal_pose_2d,2,0,0)]).
%indigolog_plan([navigate_generic(lookat_point_3d,2.0,0,0.8)]).
%indigolog_plan([navigate_generic(lookat_point_3d,4.5,2.2,0)]).
%indigolog_plan([navigate_generic(lookat_point_3d,0.806845715361795,2.1867359796956563,0.5493168002464702)]).  % = coke in gazebo, difficult location
%indigolog_plan([navigate_generic(prepare_grasp_orientation,left,1.22, -1.1, 0.8)]).

%% Perception
%indigolog_plan([perception_recognition(object,2.5)]).
%indigolog_plan([perception_recognition(face,2.5)]).
%indigolog_plan([perception_recognition(object_and_face,2.5)]).
%indigolog_plan([perception_recognition(laser_2d,2.5,1.2,-1.0,1.0)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Action query in amigo-console %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Plan with multiple actions
%% Plan examples:

%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("say","'give me a drink'"),Compound("gripper","left","open"),Compound("say","'pretty please'")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("say","'haha what a joke'"),Compound("navigate_generic","goal_pose_2d","2","0","0")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("say","'do a step forward'"),Compound("navigate_generic","goal_pose_2d","2","0","0"), Compound("say","'do a step backward'"),Compound("navigate_generic","goal_pose_2d","0","0","3.14"),Compound("say","'yeah i can do it'")))))

%%% Plan with single action

%% Human interaction
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("say","'give me a drink'")))))

%% Arms
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","lift","left")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","lift","right")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","retract","left")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","retract","right")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","carrying","left")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","carrying","right")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","prepare_grasp","left")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","to_pre_grasp_point","left","1","0","1.5")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("arm","grasp","left","1","0","1.5")))))

%% Grippers
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","left","open")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","left","close")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","right","open")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","right","close")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","both","open")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("gripper","both","close")))))

%% Head
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("head","reset")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("head","send_goal","4.5","2","1")))))

%% Spindle
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","reset","20")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","high","20")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","medium","20")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","low","20")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","send_goal","20","1")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("spindle","send_goal_laser","30","0.8")))))

%% Navigation
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("navigate_generic","goal_pose_2d","2","0","0")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("navigate_generic","lookat_point_3d","left","4.5","2.2","0.8")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("navigate_generic","prepare_grasp_orientation","left","4.5","2.2","0.8")))))

%% Perception
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("perception_recognition","object","2.5")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("perception_recognition","face","2.5")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("perception_recognition","object_and_face","2.5")))))
%amigo.reasoner.query(Compound("assertz",Compound("indigolog_plan", Sequence(Compound("perception_recognition","laser_2d","2.5","1.2","-1.0","1.0")))))



%amigo.reasoner.query(Compound("assertz",Compound("executing_action", Sequence(Compound("arm","prepare_grasp","left")))))

