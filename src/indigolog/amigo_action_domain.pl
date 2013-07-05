%% Interface to the outside world via read and write
execute(A,Sr) :- ask_execute(A,Sr).
%% exog_occurs(A) :- ask_exog_occurs(A).
% exog_occurs(_) :- fail.

side(Side) :- Side=left; Side=right.
gripper_state(State) :- State=open; State=close.
binary(V) :- V=0; V=1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HMI

%% -- say('input sentence in lower case!')
prim_action(say(_)). %% String, say('hello')
poss(say(_), true).

%% -- ask_action(+Timeout)
prim_action(ask_action(_)).
poss(ask_action(_), true).

%% -- set_rgb_lights(+R, +G, +B)
prim_action(set_rgb_lights(R, G, B)) :- domain(R, binary), domain(G, binary), domain(B, binary).
poss(set_rgb_lights(_, _, _), true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRIPPERS

%% -- gripper_state(Side) --> State
% prim_fluent(gripper_state(Side)) :- domain(Side, side).
% initally(gripper_state(_), open).

%% --set_gripper(+Side, +State)
prim_action(set_gripper(Side, State)) :- domain(Side, side), domain(State, gripper_state).
poss(set_gripper(_, _), true).
%% causes_val(set_gripper(Side, State), gripper_state(Side), State, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARMS

%% -- tuck_arm(Side)
%%    send gripper to a safe pre-grasp pose close to body
prim_action(tuck_arm(Side)) :- domain(Side, side).
poss(tuck_arm(_), true).

%% -- arm(prepare_grasp, Side)
%%    same as tuck_arm
prim_action(arm(prepare_grasp, Side)) :- domain(Side, side).
poss(arm(prepare_grasp, _), true).

%% -- arm(lift, Side)
%%    lift arm 10cms
prim_action(arm(lift, Side)) :- domain(Side, side).
poss(arm(lift, _), true).

%% -- arm(retract, Side)
%%    retract arm 10cms 
prim_action(arm(retract, Side)) :- domain(Side, side).
poss(arm(retract, _), true).

%% -- arm(carrying, Side)
%%    move arm into carrying pose
prim_action(arm(carrying, Side)) :- domain(Side, side).
poss(arm(carrying, _), true).

%% -- arm(to_pre_grasp_point, Side, X, Y, Z)
%%    set gripper to a pre-grasp position for object at point
prim_action(arm(to_pre_grasp_point, Side, _, _, _)) :- domain(Side, side).
poss(arm(to_pre_grasp_point, _, _, _, _), true).

%% -- arm(grasp, Side, X, Y, Z)
%%    move arm gently to point (X, Y, Z)
prim_action(arm(grasp, Side, _, _, _)) :- domain(Side, side).
poss(arm(grasp, _, _, _, _), true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPINDLE

spindle_range(Range) :- member(Range, [reset, high, medium, low]).

%% -- spindle(Range, Timeout)
%%    set spindle to default/hightest/medium/lowest position with Timeout
%%    Range = reset/high/medium/low
prim_action(spindle(Range, _)) :- domain(Range, spindle_range).
poss(spindle(Range, _), true) :- domain(Range, spindle_range).

%% -- spindle(send_goal, Timeout, Height)
%%    set spindle to look at point (X, Y, Height)
prim_action(spindle(send_goal, _, _)).
poss(spindle(send_goal, _, _), true).

%% -- spindle(send_goal_laser, Timeout, Height)
%%    set spindle to Height to update object position with laser
%%    TODO: make it a sensing action???
prim_action(spindle(send_goal_laser, _, _)).
poss(spindle(send_goal_laser, _, _), true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HEAD

%% -- head(reset)
%%    reset head position
prim_action(head(reset)).
poss(head(reset), true).

%% -- head(send_goal, X, Y, Z)
%%    look at point X, Y, Z
prim_action(head(send_goal, _, _, _)).
poss(head(send_goal, _, _, _), true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NAVIGATION

%% -- navigate_generic(goal_pose_2d, X, Y, Phi)
%%    navigate to a (X, Y, Phi) position
prim_action(navigate_generic(goal_pose_2d, _, _, _)).
poss(navigate_generic(goal_pose_2d, _, _, _), true).

%% -- navigate_generic(lookat_point_3d, X, Y, Z)
%%    navigate to a pose determined for looking at point (X, Y, Z)
prim_action(navigate_generic(lookat_point_3d, _, _, _)).
poss(navigate_generic(lookat_point_3d, _, _, _), true).

%% -- navigate_generic(prepare_grasp_orientation, Side, X, Y, Z)
%%    navigate to a pre-grasp-pose determined for grasping object at point (X, Y, Z)
prim_action(navigate_generic(prepare_grasp_orientation, Side, _, _, _)) :- domain(Side, side).
poss(navigate_generic(prepare_grasp_orientation, _, _, _, _), true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PERCEPTION

perception_type(Type) :- member(Type, [object, face, object_and_face]).

%% -- perception_recognition(Type, Timeout)
%%    recognize object(s)/face(s)/both
prim_action(perception_recognition(Type, _)) :- domain(Type, perception_type).
poss(perception_recognition(_, _), true).

%% -- perception_recognition(laser_2d, Timeout, X, Y, Z)
%%    update object loc at point (X, Y, Z) with the laser scanner
prim_action(perception_recognition(laser_2d, _, _, _, _)).
poss(perception_recognition(laser_2d, _, _, _, _), true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MIGRATION OF STATEMACHINE



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QUERY
obj_loc_Q(X, Y, Z) :-
	property_expected(ObjectID, position, in_front_of(amigo)),
	property_expected(ObjectID, position, [X,Y,Z]).

%% proc(demo_seq_test, [navigate_generic(goal_pose_2d, 2,0, 3), spindle(medium, 10)]).

%% proc(demo_query_test, [query(object_roi(coke-1, X, Y, Z), _), navigate_generic(goal_pose_2d, X, Y, Z)]).

proc(demo_seq_test,  [head(send_goal, 1, 2, 0.9), perception_recognition(object, 3), query(obj_loc_Q(X, Y, Z), _),  navigate_generic(lookat_point_3d,X,Y,Z),  spindle(send_goal,5,Z), head(send_goal, X, Y, Z), perception_recognition(object, 3), query(obj_loc_Q(Nx, Ny, Nz), _),  arm(to_pre_grasp_point,right,Nx,Ny,Nz),  set_gripper(right, open),  arm(grasp,right,Nx,Ny,Nz), set_gripper(right, close), arm(retract, right)]).  
% %%% Migration of Find object and Grasp object state machine

% :-dynamic object_loc/4.
% :-dynamic object_roi/4.
% :-dynamic visited/3.

% % object_loc(coke-1, 1.0, 2.2, 3.2).
object_roi(coke-1, 1.0, 2.0, 3.0).
% object_roi(coke-1, 2.0, 3.0, 4.0).

% prim_fluent(object_foundp).
% initially(object_foundp, false).

% % prim_action(senseFound(_)). %% give object ID as input
% % poss(senseFound(_), true).
% % senses(senseFound(_), object_loc).

% prim_action(queryFound(ID, X, Y, Z)).
% poss(queryFound(_, _, _, _), true).
% causes_val(queryFound(ID, X, Y, Z), object_foundp, true, call(object_loc(ID, X, Y, Z))).

% find_next_roi(ID, X, Y, Z) :-
%         object_roi(ID, X, Y, Z), \+ visited(X, Y, Z).

% prim_action(queryROI(ID, X, Y, Z)).
% poss(queryROI(_, _, _, _), true).
% causes_val(queryROI(ID, X, Y, Z), object_foundp, false, call(find_next_roi(ID, X, Y, Z))).

% proc(search_object_ROI(ID), [queryROI(ID, X, Y, Z), if(ground(X), [navigate_lookpoint(X, Y, Z), move_head(down, 20), move_spindle(70), perception_object(4), find_object(ID)], say('Sorry, I have tried but I can not find it!'))]).

% %% find_object state machine!!!!
% proc(find_object(ID), [queryFound(ID, X, Y, Z), if(object_foundp, say('I find it.'), search_object_ROI(ID))]).

% % proc(update_object_pose, )

% %% grasp_object state machine!!!!
% % proc(grasp_object(ID), [queryFound(ID, X, Y, Z), navigate_lookpoint(X, Y, Z), pi(side, set_gripper(side, open)), move_spindle(Z), ]).

