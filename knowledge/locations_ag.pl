%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                                             INIT
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % % % % % % % % % dynamic predicates % % % % % % % % % % % 
%
% Can be asserted and retracted
%
:- dynamic challenge/1.
:- dynamic environment/1.
:- dynamic state/2.
:- dynamic explored/1.
:- dynamic visited/1.
:- dynamic unreachable/1.
:- dynamic disposed/1.
:- dynamic at/2.                 % Used in demo_challenge, to say who owns a breakfasttable-spot
:- dynamic registered/1.
:- dynamic point_of_interest/4.

:- dynamic position_asserted/2.  % is asserted in case of back-up scenario for not finding bowl
:- retractall(position_asserted(_, _)).
position(A, B) :-
    position_asserted(A, B).

% % % % % % % % % % % multifile predicates % % % % % % % % % % % 
%
% Can be defined in multiple prolog files
%
:- multifile waypoint/2.
:- multifile waypoint/4.

:- multifile storage_class/2.
:- multifile storage_class/4.
:- multifile instance_of/2.
:- multifile instance_of/4.
:- multifile point_of_interest/2.
:- multifile point_of_interest/4.
:- multifile region_of_interest/1.
:- multifile region_of_interest/5.
:- multifile exploration_target/2.
:- multifile exploration_target/4.

% Used in EGPSR (Erik)
:- multifile search_query/2.
:- multifile object_query/1.
:- multifile point_location/2.

:- retractall(challenge(_)).
:- retractall(explored(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                                    ENVIRONMENT: tue_testlab_A
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % % % % GENERIC % % % % % %

waypoint(robotics_testlab_A, _, entry_point(a), pose_2d(0.139, 0.179, -0.7)).  
waypoint(robotics_testlab_A, _, entry_point(b), pose_2d(1.601, 0.512, -1.9)). 
waypoint(robotics_testlab_A, _, entry_point(c), pose_2d(0.764, 0.139, -1.6)). 
waypoint(robotics_testlab_A, _, entry_point(d), pose_2d(1.781, 0.023, -2.6)). 

% QUESTION: Who uses meeting_point (without letter)?                                    
waypoint(robotics_testlab_A, _, meeting_point, 	  pose_2d(1.293, -0.245,   0.914)). % used in cleanup if i do ctrl-f (erik) and in highlevel -> class GotoMeetingPoint (used in several executives)
waypoint(robotics_testlab_A, _, meeting_point(a), pose_2d(0.139, 0.179, -0.7)).  
waypoint(robotics_testlab_A, _, meeting_point(b), pose_2d(1.601, 0.512, -1.9)). 
waypoint(robotics_testlab_A, _, meeting_point(c), pose_2d(0.764, 0.139, -1.6)). 
waypoint(robotics_testlab_A, _, meeting_point(d), pose_2d(1.781, 0.023, -2.6)). 


waypoint(robotics_testlab_A, _, exit,                pose_2d(0,       0, -3.14)).

% QUESTION: Who uses one of these exits?
waypoint(robotics_testlab_A, _, exit_1,              pose_2d(0,       0, -3.14)).       % used in EGPSR/Emergency (Erik)
waypoint(robotics_testlab_A, _, exit_2,              pose_2d(0.5,       0, -3.14)).     % used in EGPSR/Emergency (Erik)
waypoint(robotics_testlab_A, _, exit_3,              pose_2d(-0.5,       0, -3.14)).    % used in EGPSR (Erik)
waypoint(robotics_testlab_A, _, exitB,               pose_2d(0,       0, 0)).           % ctrl-f in robocup -> cleanup/demo_challenge (Erik)
waypoint(robotics_testlab_A, _, front_of_exit,       pose_2d(0.75,       0, -3.14)).    % used in Emergency (Erik)

waypoint(robotics_testlab_A, _, initial,  		    pose_2d(0,	    0,	    0)).
waypoint(robotics_testlab_A, _, initial_egpsr_1,    pose_2d(0,	    0,	    0)).
waypoint(robotics_testlab_A, _, cabinet_expedit_1, 	pose_2d(4.952,  1.351,  1.570)).
waypoint(robotics_testlab_A, _, bed_1,             	pose_2d(6.058, -1.598,  3.113)).
waypoint(robotics_testlab_A, _, bed_2,             	pose_2d(4.0, -1.6,  0.0)).
waypoint(robotics_testlab_A, _, bed_cabinet_1,     	pose_2d(3.797, -1.240, -1.608)).
waypoint(robotics_testlab_A, _, dinner_table_1,		pose_2d(0.0, 0.0, -1.57)).
waypoint(robotics_testlab_A, _, desk_1,				pose_2d(1.75, 0.0, -1.57)).
waypoint(robotics_testlab_A, _, couch_table_1,		pose_2d(0.5, 0.0, 1.57)).
waypoint(robotics_testlab_A, _, couch_table_2, 		pose_2d(1.0, 0.0, 1.57)).
waypoint(robotics_testlab_A, _, lounge_chair_1,		pose_2d(1.8, 1.77, 3.14)).

point_of_interest(robotics_testlab_A, _, cabinet_expedit_1, point_3d(4.807,  2.102, 1.000)).
point_of_interest(robotics_testlab_A, _, bed_1,             point_3d(5.009, -1.706, 0.600)).
point_of_interest(robotics_testlab_A, _, bed_2,             point_3d(5.009, -1.706, 0.600)).
point_of_interest(robotics_testlab_A, _, bed_cabinet_1,     point_3d(3.729, -2.286, 0.700)).
point_of_interest(robotics_testlab_A, _, dinner_table_1,    point_3d(0.0,   -1.0,   1.000)).
point_of_interest(robotics_testlab_A, _, desk_1,		    point_3d(1.3,   -1.0,   0.88)).
point_of_interest(robotics_testlab_A, _, couch_table_1,     point_3d(0.5,    1.0,   0.500)).
point_of_interest(robotics_testlab_A, _, couch_table_2,     point_3d(1.0,    1.0,   0.500)).
point_of_interest(robotics_testlab_A, _, lounge_chair_1,    point_3d(1.0,    1.77,  1.000)).
point_of_interest(robotics_testlab_A, _, trashbin1,         point_3d(6.22,   0.78,   0.8)).


% % % % % % REGISTRATION % % % % % %

waypoint(robotics_testlab_A, _, registration_table1,	pose_2d(1.33,    -0.3, -1.57)). 
waypoint(robotics_testlab_A, _, registration_table2,	pose_2d(0.63,    -0.38, -1.5)).
waypoint(robotics_testlab_A, _, registration_table3,	pose_2d(1.95,    -0.24, -1.8)). 

% in rips, exit could be different kind of exit
waypoint(robotics_testlab_A, _, exit_1_rips,         pose_2d(0,       0, -3.14)).        
waypoint(robotics_testlab_A, _, exit_2_rips,         pose_2d(0.3,       0, -3.14)).
waypoint(robotics_testlab_A, _, exit_3_rips,         pose_2d(0.7,       0, -3.14)).


% % % % % % CLEAN UP % % % % % %

waypoint(robotics_testlab_A, clean_up, meeting_point, pose_2d(1.293, -0.445, 0.914)).
waypoint(robotics_testlab_A, clean_up, bedroom, pose_2d(4.315, 1.141, -0.841)).
waypoint(robotics_testlab_A, clean_up, living_room, pose_2d(2.0, 0.0, 2.4)).

exploration_target(robotics_testlab_A, clean_up, bedroom, bed_cabinet_1).
exploration_target(robotics_testlab_A, clean_up, bedroom, cabinet_expedit_1).
exploration_target(robotics_testlab_A, clean_up, bedroom, bed_1).
exploration_target(robotics_testlab_A, clean_up, bedroom, bed_2).
exploration_target(robotics_testlab_A, clean_up, living_room, dinner_table_1).
exploration_target(robotics_testlab_A, clean_up, living_room, desk_1).
exploration_target(robotics_testlab_A, clean_up, living_room, couch_table_1).
exploration_target(robotics_testlab_A, clean_up, living_room, couch_table_2).
exploration_target(robotics_testlab_A, clean_up, living_room, lounge_chair_1).

% specifies which point the robot is going to use to place an object (based on best reachable base pose)
:- dynamic current_dropoff_point/1.

%dropoff_point(robotics_testlab_A, clean_up, sidetable, ).
dropoff_point(robotics_testlab_A, clean_up, couchtable, point_3d(0.5,    1.0,   0.500)).
dropoff_point(robotics_testlab_A, clean_up, couchtable, point_3d(1.0,    1.0,   0.500)).
dropoff_point(robotics_testlab_A, clean_up, couchtable, point_3d(1.106,  1.143, 0.500)).
dropoff_point(robotics_testlab_A, clean_up, couchtable, point_3d(1.062,  0.794,   0.500)).
dropoff_point(robotics_testlab_A, clean_up, couchtable, point_3d(0.002,  0.834,   0.500)).
dropoff_point(robotics_testlab_A, clean_up, desk,       point_3d(1.80,   -1.0,   0.88)).
dropoff_point(robotics_testlab_A, clean_up, bed,        point_3d(5.009, -1.706, 0.600)).
dropoff_point(robotics_testlab_A, clean_up, trashbin,   point_3d(6.22,   0.78,   0.8)).


instance_of(john, person).
instance_of(pete, person).

owner(bed_1, john).
owner(bed_2, pete).
owner(couch_1, pete).
owner(breakfast_1, john).
owner(breakfast_2, pete).

at(john, bed_1).
at(pete, couch_1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                                        WORLD LOGIC
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

waypoint(Name, Pose) :-
    challenge(Challenge),
    environment(Env),
    waypoint(Env, Challenge, Name, Pose).

exploration_target(Room, Target) :-
    challenge(Challenge),
    environment(Env),
    exploration_target(Env, Challenge, Room, Target).
    
point_of_interest(ROI, Pose) :-
    challenge(Challenge),
    environment(Env),
    point_of_interest(Env, Challenge, ROI, Pose).

% QUESTION: Who uses this?
region_of_interest(Loc, Point, Size) :-
    challenge(Challenge),
    environment(Env),
    region_of_interest(Env, Challenge, Loc, Point, Size).
    
point_of_interest_emergency(ROI, Pose) :-
    challenge(Challenge),
    environment(Env),
    ROI=..[apartment,_],
    point_of_interest(Env, Challenge, ROI, Pose).
   
storage_class(X, Y) :-
    challenge(Challenge),
    environment(Env),
    storage_class(Env, Challenge, X, Y).
    
instance_of(X, Y) :-
    challenge(Challenge),
    environment(Env),
    instance_of(Env, Challenge, X, Y).

dropoff_point(X, Y) :-
    challenge(Challenge),
    environment(Env),
    dropoff_point(Env, Challenge, X, Y).

% QUESTION: What is the difference between instance_of/4 and static_instance_of/4
instance_of(X, Y) :-
    challenge(Challenge),
    environment(Env),
    static_instance_of(Env, Challenge, X, Y).

% QUESTION: Who uses this?                  % I guess I did in emergency before. But now I do not any more. Can be deleted i guess. (erik)
person_query(ObjectID, [X,Y,Z]) :-
    property_expected(ObjectID, class_label, person),
    property_expected(ObjectID, position, [X,Y,Z]),
    not(registered(ObjectID)).