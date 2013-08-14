#!/usr/bin/python
import roslib; roslib.load_manifest('atomic_grounding_executioner')

import rospy
import robot_skills
import robot_skills.amigo
from psi import *                   # To use for example Compound and Conjunction
from std_msgs.msg import String     # For .get_string()
import robot_smach_states as states
import smach
import smach_ros
import geometry_msgs

def startup():
	amigo = robot_skills.amigo.Amigo(wait_services=True)
	return amigo

def initialize(self):
    self.lights.set_color(0,0,1)  #be sure lights are blue

    ## self.head.reset_position()
    self.leftArm.reset_arm()
    self.leftArm.send_gripper_goal_close()
    self.rightArm.reset_arm()
    self.rightArm.send_gripper_goal_close()
    self.reasoner.reset()
    self.spindle.reset()

    ## Check if TF link between /map and /base_link is set, if not error at initialize in stead of during first navigate execution
    rospy.loginfo("[AG] TF link between /map and /base_link is checked. If it takes longer than a second, probably an error. Do a restart!!!")
    self.base.get_location()

def init_retract_facts(self):

	##retract old facts
    ## commented because the interpreter will do this everytime you query a new plan
    # robot.reasoner.query(Compound("retractall", Compound("indigolog_plan", "X")))
    # robot.reasoner.query(Compound("retractall", Compound("action", "X")))

    ##Load database
    self.reasoner.query(Compound("load_database","tue_owls_indigolog_exec",'knowledge/kb_ag_1.pl'))
    self.reasoner.query(Compound("load_database", "tue_owls_indigolog_exec", 'knowledge/locations_ag.pl'))
    rospy.loginfo("[AG] Database loaded")

    self.reasoner.query(Compound("load_database","tue_owls_indigolog_exec",'src/indigolog/main.pl'))
    rospy.loginfo("[AG] Indigolog loaded.")

def execute_smach_statemachine(self, machine):

    introserver = smach_ros.IntrospectionServer('server_name', machine, '/SM_ROOT_PRIMARY')
    introserver.start()
    try:
        result = machine.execute()
    except Exception, e:
        self.speech.speak(e)
    finally:
        introserver.stop()

    return result

def execute_smach_state(self, state, transitions):

    class Generated_StateMachine(smach.StateMachine):
        def __init__(self, state, transitions):
            smach.StateMachine.__init__(self,outcomes=transitions)

            with self:

                smach.StateMachine.add('EXECUTE_STATE', state)

    machine = Generated_StateMachine(state, transitions)

    introserver = smach_ros.IntrospectionServer('server_name', machine, '/SM_ROOT_PRIMARY')
    introserver.start()
    try:
        result = machine.execute()
    except Exception, e:
        self.speech.speak(e)
    finally:
        introserver.stop()

    return result

def execute(self, action, action_input=None):

    ### HUMAN INTERACTION ###

    if action == "say":
        sentence = str(action_input[0])
        self.speech.speak(sentence)

    ### ARMS ###

    elif action == "arm":
        arm_action = str(action_input[0])
        side = str(action_input[1])
        if side == "left":
            robotArm = robot.leftArm
            self.arm = robot.leftArm
        elif side == "right":
            robotArm = robot.rightArm
            self.arm = robot.rightArm

        if arm_action == "prepare_grasp":
            self.arm.send_joint_goal(-0.2, -0.044, 0.69, 1.4, -0.13, 0.38, 0.42)

        #rospy.loginfo("[AG] perception_action = '{0}'".format(perception_action))
        if arm_action == "lift":
            state = states.ArmToUserPose(robotArm, 0.0, 0.0, 0.1, 0.0, 0.0 , 0.0, time_out=20, pre_grasp=False, frame_id="/base_link", delta=True)
            transitions = ['succeeded', 'failed']

        elif arm_action == "retract":
            state = states.ArmToUserPose(robotArm, -0.1, 0.0, 0.0, 0.0, 0.0, 0.0, time_out=20, pre_grasp=False, frame_id="/base_link", delta=True)
            transitions = ['succeeded', 'failed']

        elif arm_action == "carrying":
            if side == "left":
                y_home = 0.2
            elif side == "right":
                y_home = -0.2
            self.arm.send_goal(0.18, y_home, 0.75, 0, 0, 0, time_out=60)

        elif arm_action == "to_pre_grasp_point":
            # if arm = both -> failed
            goal_map = geometry_msgs.msg.Point()
            goal_map.x, goal_map.y, goal_map.z = float(action_input[2]), float(action_input[3]), float(action_input[4])

            goal_bl = states.util.transformations.tf_transform(goal_map, "/map", "/base_link", tf_listener=self.tf_listener)
            if goal_bl == None:
                result = "failed"
            elif self.arm.send_goal(goal_bl.x, goal_bl.y, goal_bl.z, 0, 0, 0,
                frame_id="/base_link",
                time_out=20,
                pre_grasp=True,
                first_joint_pos_only=True):
                result = "succeeded"
            else:
                rospy.loginfo("[AG] Sending goal to {0}".format(goal_bl))
                result = "failed"

        elif arm_action == "grasp":
            x,y,z = (float(action_input[2]), float(action_input[3]), float(action_input[4]))

            self.reasoner.query(Compound("retractall", Compound("atomic_arm_grasppoint", "X", "Y", "Z")))
            self.reasoner.assertz(Compound("atomic_arm_grasppoint", x, y, z))

            query_grasp = Compound("atomic_arm_grasppoint","X", "Y", "Z")

            state = states.Grab(robotArm, self, query_grasp)
            transitions = ['grab_succeeded', 'grab_failed','target_lost']

        elif arm_action == "drop_off":
            x,y,z = (float(action_input[2]), float(action_input[3]), float(action_input[4]))

            self.reasoner.query(Compound("retractall", Compound("atomic_arm_drop_off", "X", "Y", "Z")))
            self.reasoner.assertz(Compound("atomic_arm_drop_off", x, y, z))

            query_drop_off = Compound("atomic_arm_drop_off","X", "Y", "Z")

            state = states.DropObject(robotArm, self, query_drop_off)
            transitions = ['succeeded', 'failed','target_lost']

        #elif arm_action == "to_joint_pose"  TODO!!

        if arm_action == "lift" or arm_action == "retract" or arm_action == "grasp" or arm_action == "drop_off":
            result = execute_smach_state(self, state, transitions)
        if arm_action == "lift" or arm_action == "retract" or arm_action == "to_pre_grasp_point" or arm_action == "drop_off":            
            if result == "succeeded":
                rospy.loginfo("[AG] Arm movement succeeded.")
                result = "success"
            elif result == "failed":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Failed arm movement.")
                result = "failed"
            elif result == "target_lost":  # also possible at drop_off
                rospy.loginfo("[AG] Failed arm movement (target lost).")
                result = "failed"
        elif arm_action == "grasp":
            if result == "grab_succeeded":
                rospy.loginfo("[AG] Grasp succeeded.")
            elif result == "grab_failed":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Grasp failed.")
            elif result == "target_lost":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Grasp: target lost")

        elif not (arm_action == "prepare_grasp"):
            rospy.logwarn("[AG] Arm action name is wrong! Have a look at it!")

    ### GRIPPERS ###

    elif action == "gripper":

        side = str(action_input[0])
        state = str(action_input[1])

        if side == "left" and state == "open":
            self.leftArm.send_gripper_goal(0)      #open
        elif side == "left" and state == "close":
            self.leftArm.send_gripper_goal(1)      #close
        elif side == "right" and state == "open":
            self.rightArm.send_gripper_goal(0)      #open
        elif side == "right" and state == "close":
            self.rightArm.send_gripper_goal(1)      #close
        elif side == "both" and state == "open":
            self.leftArm.send_gripper_goal(0)      #open
            self.rightArm.send_gripper_goal(0)     #open
        elif side == "both" and state == "close":
            self.leftArm.send_gripper_goal(1)      #open
            self.rightArm.send_gripper_goal(1)     #open

    ### HEAD ###

    elif action == "head":
        head_action = str(action_input[0])

        if head_action == "reset":
            self.head.reset_position()
        elif head_action == "send_goal":
            x,y,z = float(action_input[1]), float(action_input[2]), float(action_input[3])
            lookat_point = self.head.point(x,y,z)

            self.head.send_goal(lookat_point)

        # Check if correct action names have been used:
        if not (head_action == "reset" or head_action == "send_goal"):
            rospy.logwarn("[AG] Head action name is wrong! Have a look at it!")

    ### SPINDLE ###

    elif action == "spindle":
        spindle_action = str(action_input[0])
        waittime = float(action_input[1])


        if spindle_action == "reset":
            self.spindle.reset()
        elif spindle_action == "send_goal":
            z = float(action_input[2])
            rospy.logdebug("self.spindle.upper_limit = {0}".format(self.spindle.upper_limit))
            spindle_target = max(0.15, min(z - 0.41, self.spindle.upper_limit))
            self.spindle.send_goal(spindle_target,waittime=waittime)
        elif spindle_action == "send_goal_laser":
            z = float(action_input[2])
            self.spindle.send_laser_goal(z,waittime)
        elif spindle_action == "high":
            self.spindle.send_goal(self.spindle.upper_limit, waittime=waittime)
        elif spindle_action == "medium":
            self.spindle.send_goal((self.spindle.upper_limit + self.spindle.lower_limit)/2, waittime=waittime)
        elif spindle_action == "low":
            self.spindle.send_goal(self.spindle.lower_limit, waittime=waittime)

        # Check if correct action names have been used:
        if not (spindle_action == "reset" or spindle_action == "send_goal" or spindle_action == "send_goal_laser" or spindle_action == "high" or spindle_action == "medium" or spindle_action == "low"):
            rospy.logwarn("[AG] Spindle action name is wrong! Have a look at it!")

    ### NAVIGATION ###

    elif action == "navigate_generic":
        nav_action = str(action_input[0])

        #rospy.loginfo("[AG] action = '{0}'".format(nav_action))
        if nav_action == "goal_pose_2d":
            goal = (float(action_input[1]), float(action_input[2]), float(action_input[3]))
            machine = states.NavigateGeneric(self, goal_pose_2d=goal)
        elif nav_action == "lookat_point_3d":
            goal = (float(action_input[1]), float(action_input[2]), float(action_input[3]))
            machine = states.NavigateGeneric(self, lookat_point_3d=goal)
        elif nav_action == "prepare_grasp_orientation":
            side = str(action_input[1])
            if side == "left":
                robotArm = robot.leftArm
            elif side == "right":
                robotArm = robot.rightArm

            x,y,z = (float(action_input[2]), float(action_input[3]), float(action_input[4]))

            self.reasoner.query(Compound("retractall", Compound("atomic_navigate_grasppoint", "X", "Y", "Z")))
            self.reasoner.assertz(Compound("atomic_navigate_grasppoint", x, y, z))

            query_grasp = Compound("atomic_navigate_grasppoint","X", "Y", "Z")

            machine = states.PrepareOrientation(robotArm, self, query_grasp)

        if nav_action == "goal_pose_2d" or nav_action == "lookat_point_3d":
            result = execute_smach_statemachine(self, machine)
            if result == "unreachable" or result == "preempted":
                rospy.loginfo("[AG] Amigo: My attempt to reach a location failed")
            if result == "arrived":
                rospy.loginfo("[AG] Amigo: I am at the goal location")
            if result == "goal_not_defined":
                rospy.loginfo("[AG] Amigo: I was not able to get a goal pose")
        elif nav_action == "prepare_grasp_orientation":
            result = execute_smach_statemachine(self, machine)
            if result == "orientation_failed" or result == "abort" or result == "target_lost":
                rospy.loginfo("[AG] Amigo: My attempt to reach a location failed")
            if result == "orientation_succeeded":
                rospy.loginfo("[AG] Amigo: I am at the goal location")

        else:
            rospy.logwarn("[AG] Action name is wrong! Have a look at it!")

    ### PERCEPTION ###

    elif action == "perception_recognition":
        perception_action = str(action_input[0])

        #rospy.loginfo("[AG] perception_action = '{0}'".format(perception_action))
        if perception_action == "object":
            # Action: perception_recognition(object, time)

            time = float(action_input[1])
            rospy.loginfo("[AG] Start object recognition")
            self.perception.toggle(["template_matching"])

            # Let the object recognition run for a certain period
            rospy.sleep(time)

            rospy.loginfo("[AG] Stop object recognition")
            self.perception.toggle([])

        elif perception_action == "face":
            # Action: perception_recognition(face, time)

            time = float(action_input[1])

            rospy.loginfo("[AG] Start face recognition")
            self.perception.toggle(['face_segmentation'])

            # Let the object recognition run for a certain period
            rospy.sleep(time)

            rospy.loginfo("[AG] Stop face recognition")
            self.perception.toggle([])

        elif perception_action == "object_and_face":
            # Action: perception_recognition(object_and_face, time)

            time = float(action_input[1])

            rospy.loginfo("[AG] Start object and face recognition")
            self.perception.toggle(["template_matching","face_segmentation"])

            # Let the object recognition run for a certain period
            rospy.sleep(time)

            rospy.loginfo("[AG] Stop object and face recognition")
            self.perception.toggle([])


        elif perception_action == "laser_2d":
            # Action: perception_recognition(2d, time, x, y, z)

            time = float(action_input[1])
            x,y,z = float(action_input[2]), float(action_input[3]), float(action_input[4])

            target_point = geometry_msgs.msg.PointStamped()
            target_point.header.frame_id = "/map"
            target_point.header.stamp = rospy.Time()
            target_point.point.x = x
            target_point.point.y = y
            target_point.point.z = z

            rospy.loginfo("[AG] Start 2d recognition with laserscanner")
            self.perception.toggle(["object_detector_2d"])
            self.perception.set_perception_roi(target_point, length_x=0.5, length_y=0.5, length_z=0.3)


            rospy.logwarn("[AG] Waiting for {0} seconds for laser update".format(time))
            rospy.sleep(rospy.Duration(time))

            # Stop perception
            rospy.loginfo("[AG] Stop all perception")
            self.perception.toggle([])

        # Check if correct action names have been used:
        rospy.loginfo("[AG] perception_action = {0}".format(perception_action))
        if not (perception_action == "object" or perception_action == "face" or perception_action == "object_and_face" or perception_action == "laser_2d"):
            rospy.logwarn("[AG] Perception action name is wrong! Have a look at it!")

if __name__ == "__main__":

    rospy.init_node('atomic_grounding_executioner')
    rospy.loginfo("[AG = Atomic Grounding]")

    robot = startup()
    initialize(robot)
    # add initial pose here
    init_retract_facts(robot)

    rospy.loginfo("[AG] Initialized")
    action = "initial"

    robot.reasoner.query(Compound("indigolog","clean_up_challenge"))

    while not action == "done" and not rospy.is_shutdown():
        ## Query task list: indigolog_plan(X)
        # indigolog_plan_query = robot.reasoner.query(Compound("indigolog_plan", "X"))

        # if indigolog_plan_query:

        #     indigolog_plan = [(answer["X"]) for answer in indigolog_plan_query]
        #     plan = min(indigolog_plan)
        #     rospy.loginfo("plan is {0}".format(plan))

        # Query for indigolog_plan(X) but also action(step) during testing
        answer_query = robot.reasoner.query(Compound("get_action", "Action", "Action_input"))

        if not answer_query:
            rospy.loginfo("[AG] No action found")

        else:
            rospy.loginfo("[AG] Action found!!")

            action_answers = [(answer["Action"], answer["Action_input"]) for answer in answer_query]
            #rospy.loginfo("[AG] action_answers = {0}".format(action_answers))
            action, action_input = action_answers[0]

            rospy.loginfo("[AG] Action to perform = {0} ".format(action))
            rospy.loginfo("[AG] Detail action = {0} ".format(action_input[0]))

            action = action.get_string()

            execute(robot, action, action_input)

            rospy.loginfo("[AG] Asserting success")
            #robot.reasoner.query(Compound("assert_done", "success", action, action_input))
            robot.reasoner.query(Compound("assert_done", "success"))

            rospy.loginfo("[AG] Ready for next action")

        rospy.sleep(1)
