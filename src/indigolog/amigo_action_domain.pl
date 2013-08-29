%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% General facts

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

%% -- set_rgb_lights(+R,+G,+B)
prim_action(set_rgb_lights(R,G,B)) :-
        domain(R,binary), domain(G,binary), domain(B,binary).
poss(set_rgb_lights(_,_,_),true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRIPPERS

%% -- gripper_state(Side) --> State
% prim_fluent(gripper_state(Side)) :- domain(Side, side).
% initally(gripper_state(_), open).

%% -- gripper(+Side, +State)
prim_action(gripper(Side,State)) :-
        domain(Side,side), domain(State,gripper_state).
poss(gripper(_, _), true).
%% causes_val(gripper(Side, State), gripper_state(Side), State, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARMS

%% -- tuck_arm(Side)
%%    send gripper to a safe pre-grasp pose close to body
prim_action(tuck_arm(Side)) :- domain(Side,side).
poss(tuck_arm(_),true).

%% -- arm(prepare_grasp, Side)
%%    same as tuck_arm
prim_action(arm(prepare_grasp,Side)) :- domain(Side,side).
poss(arm(prepare_grasp, _), true).

%% -- arm(lift, Side)
%%    lift arm 10cms
prim_action(arm(lift,Side)) :- domain(Side,side).
poss(arm(lift, _), true).

%% -- arm(retract, Side)
%%    retract arm 10cms 
prim_action(arm(retract,Side)) :- domain(Side,side).
poss(arm(retract, _), true).

%% -- arm(carrying, Side)
%%    move arm into carrying pose
prim_action(arm(carrying,Side)) :- domain(Side,side).
poss(arm(carrying, _), true).

%% -- arm(to_pre_grasp_point, Side, X, Y, Z)
%%    set gripper to a pre-grasp position for object at point
prim_action(arm(to_pre_grasp_point, Side, _, _, _)) :- domain(Side, side).
poss(arm(to_pre_grasp_point, _, _, _, _), true).

%% -- arm(grasp, Side, X, Y, Z)
%%    move arm gently to point (X, Y, Z)
prim_action(arm(grasp, Side, _, _, _)) :- domain(Side, side).
poss(arm(grasp, _, _, _, _), true).

%% -- arm(dropoff,Side,X,Y,Z)
%%    move the gripper to (X,Y,Z) to drop off the object 
prim_action(arm(drop_off,Side,_,_,_)) :- domain(Side,side).
poss(arm(drop_off,_,_,_,_),true).

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

%% -- navigate_generic(goal_pose_2d,X,Y,Phi)
%%    navigate to a (X, Y, Phi) position
prim_action(navigate_generic(goal_pose_2d,_,_,_)).
poss(navigate_generic(goal_pose_2d,_,_,_),true).
causes_val(navigate_generic(goal_pose_2d,X,Y,Phi),explored_loc_f(X,Y,Phi),true,true).

%% -- navigate_generic(lookat_point_3d,X,Y,Z)
%%    navigate to a pose determined for looking at point (X,Y,Z)
prim_action(navigate_generic(lookat_point_3d,_,_,_)).
poss(navigate_generic(lookat_point_3d, _, _, _),true).
causes_val(navigate_generic(lookat_point_3d,X,Y,Z),explored_loc_f(X,Y,Z),true,true).
%% causes_val(navigate_generic(lookat_point_3d,X,Y,_),explored_target_f(loc_id_f),true,true).

%% -- navigate_generic(prepare_grasp_orientation, Side, X, Y, Z)
%%    navigate to a pre-grasp-pose determined for grasping object at point (X, Y, Z)
prim_action(navigate_generic(prepare_grasp_orientation,Side,_,_,_)) :-
        domain(Side,side).
poss(navigate_generic(prepare_grasp_orientation,_,_,_,_),true).
causes_val(navigate_generic(prepare_grasp_orientation,_,X,Y,_),explored_loc_f(X,Y,_),true,true).

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


prim_fluent(obj_id_f).
initially(obj_id_f, unknown).
prim_fluent(loc_id_f).
initially(loc_id_f, unknown).
prim_fluent(ppl_id_f).
initially(ppl_id_f, unknown).

prim_fluent(point_x_f).
prim_fluent(point_y_f).
prim_fluent(point_z_f).

prim_fluent(pose_x_f).
prim_fluent(pose_y_f).
prim_fluent(pose_z_f).

prim_fluent(task_done_f).
initially(task_done_f,no).

prim_fluent(explored_loc_f(_,_,_)). %% pose
initially(explored_loc_f(_,_,_),false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MIGRATION OF STATEMACHINE

prim_action(query(_)).
poss(query(_),true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QUERY
obj_loc_Q(X,Y,Z) :-
        ( property_expected(ObjectID, position, in_front_of(amigo)),
	  property_expected(ObjectID, position, [TX,TY,TZ]), X is TX-0.02, Y is TY+0.01, Z is TZ-0.07;
          X=out_of_reach,Y=out_of_reach,Z=out_of_reach).

% obj_loc_Q(desk-1,1,2,3).
poi_Q(Target,X,Y,Z) :-
        point_of_interest(robotics_testlab_A,_,Target,point_3d(X,Y,Z)).

%% poi_Q(Traget,1,2,3).

%% object_roi(coke-1, 1.0, 2.0, 3.0).
%% -- navigate_Q(+Target, -X, -Y, -Phi)
%%    give a Target, return loc to go
%% navigate_Q(Target, X, Y, Phi) :- ! .

explore_Q(Target) :-
        (point_of_interest(_,_,Target,point_3d(X,Y,Z)),
        now(H), \+ has_val(explored_loc_f(X,Y,Z),true,H);
            Target = all_explored).

dp_Q(Target,X,Y,Z) :-
        dropoff_point(robotics_testlab_A,clean_up,trashbin,point_3d(X,Y,Z)).

query_map(explore_target,explore_Q(Target),[loc_id_f,Target]).
query_map(poi(Obj_ID),poi_Q(Obj_ID,X,Y,Z),[point_x_f,X,point_y_f,Y,point_z_f,Z]).
query_map(obj_loc,obj_loc_Q(X,Y,Z),[point_x_f,X,point_y_f,Y,point_z_f,Z]).
query_map(drop_point(Target),dp_Q(Target,X,Y,Z),[point_x_f,X,point_y_f,Y,point_z_f,Z]).
%% query_map(obj_loc(Obj_ID),obj_loc_Q(Obj_ID,X,Y,Z),[point_x_f,X,point_y_f,Y,point_z_f,Z]).

proc(next_loc_to_check, [query(explore_target), %% update loc_id_f
                         query(poi(loc_id_f))   %% update point x,y,z
                         ]).

proc(navigate, [navigate_generic(lookat_point_3d,point_x_f,point_y_f,point_z_f)]).

proc(check_object, [spindle(send_goal,5.0,point_z_f),
                    head(send_goal,point_x_f,point_y_f,point_z_f),
                    perception_recognition(object,2.5),
                    query(obj_loc) %% update point x,y,z
                   ]).

proc(search_trash, [next_loc_to_check,
                    navigate,
                    check_object]).

proc(adjust_grasping_pose, [arm(prepare_grasp,right),
                            spindle(send_goal,5.0,point_z_f),
                            navigate_generic(prepare_grasp_orientation,right,point_x_f,point_y_f,point_z_f),                            
                            gripper(right,open),
                            head(reset),
                            spindle(high,5.0)
             ]).

proc(grasp, [arm(to_pre_grasp_point,right,point_x_f,point_y_f,point_z_f),
             arm(grasp,right,point_x_f,point_y_f,point_z_f),
             gripper(right,close),
             arm(lift,right),
             arm(retract,right),
             arm(carrying,right)
            ]).

proc(to_drop_point, [query(poi(trashbin1)),
                     navigate_generic(prepare_grasp_orientation,right,point_x_f,point_y_f,point_z_f)
                    ]).

proc(drop_off, [query(drop_point(trashbin1)),
                arm(drop_off,right,point_x_f,point_y_f,point_z_f),
                gripper(right,open)
               ]).

proc(clean_up_challenge, [while(neg(loc_id_f=all_explored),
                                [search_trash,
                                 if(neg(point_x_f=out_of_reach),
                                    [adjust_grasping_pose,
                                     grasp,
                                     to_drop_point,
                                     drop_off],
                                    clean_up_challenge)
                                ])
                         ]).

prim_fluent(lost_obj_f).
initially(lost_obj_f,false).

guard_condition(arm(grasp,_,_,_,_), lost_obj_f=false).
rescues(arm(grasp,_,_,_,_),
        [arm(to_pre_grasp_point,right,point_x_f,point_y_f,point_z_f),
         arm(grasp,right,point_x_f,point_y_f,point_z_f)]).

exog_action(forward).
causes_val(forward,point_y_f,V,V is point_y_f+0.05).
causes_val(forward,lost_obj_f,true,true).

exog_action(backward).
causes_val(backward,point_y_f,V,V is point_y_f-0.05).
causes_val(backward,lost_obj_f,true,true).

exog_action(left).
causes_val(left,point_x_f,V,V is point_x_f-0.05).
causes_val(left,lost_obj_f,true,true).

exog_action(right).
causes_val(right,point_x_f,V,V is point_x_f+0.05).
causes_val(right,lost_obj_f,true,true).

exog_action(down).
causes_val(down,point_z_f,V,V is point_z_f-0.05).
causes_val(down,lost_obj_f,true,true).

exog_action(up).
causes_val(up,point_z_f,V,V is point_z_f+0.05).
causes_val(up,lost_obj_f,true,true).

%% reaction test
proc(demo_failure_handling, [search_trash,
                             adjust_grasping_pose,
                             grasp
                            ]).

%% par test
proc(demo_parallel_execution, [par(navigate_generic(goal_pose_2d,4.92,0.82,1.59),
                                   spindle(low,5.0))
                              ]).

% proc(grasp_obj_at(LOC), [query(poi(LOC)),
%                          navigate_generic(lookat_point_3d,point_x_f,point_y_f,point_z_f),
%                          spindle(send_goal,5,point_z_f),
%                          head(send_goal,point_x_f,point_y_f,point_z_f),
%                          perception_recognition(object,2.5),
%                          query(obj_loc),
%                          arm(prepare_grasp,left),
%                          spindle(send_goal,5,point_z_f),
%                          navigate_generic(prepare_grasp_orientation,left,point_x_f,point_y_f,point_z_f),
%                          gripper(left,open),
%                          head(reset),
%                          spindle(high,5),
%                          arm(to_pre_grasp_point,left,point_x_f,point_y_f,point_z_f),
%                          arm(grasp,left,point_x_f,point_y_f,point_z_f),
%                          gripper(left,close),
%                          arm(lift,left),
%                          arm(retract,left),
%                          arm(carrying,left)]).

