#!/usr/bin/python
import roslib; roslib.load_manifest('tue_owls_indigolog_exec')
import rospy
import robot_skills
import robot_skills.amigo
from psi import *                   # To use for example Compound and Conjunction
from std_msgs.msg import String     # For .get_string()
import robot_smach_states as states
import smach
import smach_ros
import geometry_msgs

import threading

def startup(): 
    amigo = robot_skills.amigo.Amigo(wait_services=True)
    return amigo

def initialize(self):
    self.lights.set_color(0,0,1)  #be sure lights are blue
        
    self.head.reset_position()
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
    self.reasoner.query(Compound("retractall", Compound("thread_ready_for_execution", "X","Y","Z")))    
    self.reasoner.query(Compound("retractall", Compound("thread_running", "X", "Y","Z")))
    self.reasoner.query(Compound("retractall", Compound("thread_finished_succes", "X", "Y","Z")))
    self.reasoner.query(Compound("retractall", Compound("thread_finished_failed", "X", "Y","Z")))
    self.reasoner.query(Compound("retractall", Compound("action_noted_as_finished", "X", "Y")))

    ##Load database
    self.reasoner.query(Compound("load_database","tue_owls_indigolog_exec",'knowledge/kb_ag.pl'))
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
        result = "success"

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

        #elif arm_action == "to_joint_pose"  TODO!!
    
        if arm_action == "lift" or arm_action == "retract" or arm_action == "grasp":
            result = execute_smach_state(self, state, transitions)
        if arm_action == "lift" or arm_action == "retract" or arm_action == "to_pre_grasp_point":            
            if result == "succeeded":
                rospy.loginfo("[AG] Arm movement succeeded.")
                result = "success"
            elif result == "failed":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Failed arm movement.")
                result = "failed"
        elif arm_action == "grasp":
            if result == "grab_succeeded":
                rospy.loginfo("[AG] Grasp succeeded.")
                result = "success"
            elif result == "grab_failed":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Grasp failed.")
                result = "failed"
            elif result == "target_lost":
                # exogenous event that plan failed
                rospy.loginfo("[AG] Grasp: target lost")
                result = "failed"

        elif not (arm_action == "prepare_grasp" or arm_action == "carrying"):
            rospy.logwarn("[AG] Arm action name is wrong! Have a look at it!")
        
        else:
            result = "success"


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
        result = "success"
    
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
        if not(head_action == "reset" or head_action == "send_goal"):
            rospy.logwarn("[AG] Head action name is wrong! Have a look at it!")
        result = "success"

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
        result = "success"

    ### NAVIGATION ###

    elif action == "navigate_generic": 
        nav_action = str(action_input[0])

        #rospy.loginfo("[AG] action = '{0}'".format(nav_action))
        if nav_action == "goal_pose_2d":
            goal = (float(action_input[1]), float(action_input[2]), float(action_input[3]))
            machine = states.NavigateGeneric(self, goal_pose_2d=goal)
        elif nav_action == "lookat_point_3d":
            rospy.loginfo("[AG] Lookat_point_3d -> action_input[1] = {0}, action_input[2] = {1}, action_input[3] = {2}".format(action_input[1], action_input[2], action_input[3]))
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
                result = "failed"
            if result == "arrived":
                rospy.loginfo("[AG] Amigo: I am at the goal location")
                result = "success"
            if result == "goal_not_defined":
                rospy.loginfo("[AG] Amigo: I was not able to get a goal pose")
                result = "failed"
        elif nav_action == "prepare_grasp_orientation":
            result = execute_smach_statemachine(self, machine)
            if result == "orientation_failed" or result == "abort" or result == "target_lost":
                rospy.loginfo("[AG] Amigo: My attempt to reach a location failed")
                result = "failed"
            if result == "orientation_succeeded":
                rospy.loginfo("[AG] Amigo: I am at the goal location")
                result = "success"
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
        result = "success"

    return result


class GetActionThread (threading.Thread):
    def __init__(self, threadID, name, robot):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.robot = robot
        self.action_number = 1

    def stop(self):
        self.__stop = True

    def run(self):
        print "Starting " + self.name
        action = "initial" 

        rospy.loginfo("[AG] Waiting 3 seconds for initializing to finish")
        rospy.sleep(3)

        while not action == "done" and not rospy.is_shutdown():
            #rospy.loginfo("[AG] Retracting facts for query for action")
            self.robot.reasoner.query(Compound("retract_facts_for_get_action", "X"))
            rospy.loginfo("[AG] Starting query for action")
            answer_query = self.robot.reasoner.query(Compound("get_action", "Action", "Action_input"))

            rospy.logdebug("[AG] Query for action is done.")

            if not answer_query:
                rospy.loginfo("[AG] No action found")

            else:
                action_answers = [(answer["Action"], answer["Action_input"]) for answer in answer_query]       
                rospy.loginfo("[AG] action_answers = {0}".format(action_answers))
                action, action_input = action_answers[0]

                # rospy.loginfo("[AG] TEST action = {0} ".format(action))
                # rospy.loginfo("[AG] TEST action_input = {0} ".format(action_input))

                # rospy.loginfo("[AG] TEST action[0] = {0} ".format(action[0]))
                # rospy.loginfo("[AG] TEST action_input[0] = {0} ".format(action_input[0]))

                length_action = [(answer["Length"]) for answer in self.robot.reasoner.query(Compound("get_length_list", action, "Length"))]

                rospy.loginfo("[AG] TEST length_action = {0} ".format(length_action))

                k = 0.0
                while not k == float(length_action[0]):
                    i = 0
                    j = 0.0
                    while not j == float(length_action[0]):
                        current_action = action[i]
                        current_action_input = action_input[i]
                    
                        rospy.loginfo("[AG] Check thread_running for action {0} with inputs {1}:".format(current_action, current_action_input))

                        is_action_busy   = self.robot.reasoner.query(Compound("thread_running", "X", current_action, "Y"))
                        is_action_succes = self.robot.reasoner.query(Compound("thread_finished_succes", "X", current_action, current_action_input))
                        is_action_failed = self.robot.reasoner.query(Compound("thread_finished_failed", "X", current_action, current_action_input))

                        if is_action_busy:
                            rospy.loginfo("[AG] Found action is currently active.")
                        elif is_action_succes or is_action_failed:
                            if is_action_succes:
                                rospy.loginfo("[AG] Found action has already been finished succesfully.")
                            elif is_action_failed:
                                rospy.loginfo("[AG] Found action has already been finished, but failed.")

                            answer_noted_as_finished = self.robot.reasoner.query(Compound("action_noted_as_finished", current_action, current_action_input))

                            if not answer_noted_as_finished:
                                k = k + 1.0
                                rospy.loginfo("[AG] action_noted_as_finished is added witch action {0} and action input {1}".format(current_action,current_action_input))                               
                                self.robot.reasoner.assertz(Compound("action_noted_as_finished", current_action, current_action_input))

                            rospy.loginfo("[AG] k = {0}".format(k))

                            if k == float(length_action[0]):
                               break
                        else:
                            rospy.loginfo("[AG] New action found!!")

                            rospy.loginfo("[AG] Action to perform = {0} ".format(current_action))
                            rospy.loginfo("[AG] Detail action = {0} ".format(current_action_input))

                            current_action = current_action.get_string()

                            thread = get_action_thread(robot,current_action,current_action_input)
                            thread.setDaemon(True)
                            thread.start()
                            rospy.loginfo("[AG] In case of concurrent actions, next action will be executed if possible.")

                        i=i+1
                        j=j+1.0
                        rospy.sleep(0.2)
                        
            rospy.sleep(1)
            self.robot.reasoner.query(Compound("retractall", Compound("thread_finished_succes", "X","Y","Z")))
            self.robot.reasoner.query(Compound("retractall", Compound("thread_finished_failed", "X","Y","Z")))
            self.robot.reasoner.query(Compound("retractall", Compound("action_noted_as_finished", "X","Y")))
            
        print "Exiting " + self.name


def get_action_thread(self, action, action_input):
    self.action = action
    robot = self

    if self.action == "arm":
        side = str(action_input[1])
        if side == "left":
            name = "thread_armleft"
            check_thread_running(robot,name, action, action_input)
            thread = ExecuteActionThread(2, name, robot, action, action_input)
        elif side == "right":
            name = "thread-armright"
            check_thread_running(robot,name, action, action_input)
            thread = ExecuteActionThread(3, name, robot, action, action_input)

    elif self.action == "say":
        name = "thread_say"
        check_thread_running(robot,name, action, action_input)
        thread = ExecuteActionThread(4, name , robot, action, action_input)

    elif self.action == "gripper":
        side = str(action_input[0])
        if side == "left":
            name = "thread_gripper_left"
            check_thread_running(robot,name, action, action_input)
            thread = ExecuteActionThread(5, name, robot, action, action_input)
        elif side == "right":
            name = "thread_gripper_right"
            check_thread_running(robot,name, action, action_input)
            thread = ExecuteActionThread(6, name, robot, action, action_input)

    elif self.action == "head":
        name = "thread_head"
        check_thread_running(robot,name, action, action_input)
        thread = ExecuteActionThread(7, name, robot, action, action_input)

    elif self.action == "spindle":
        name = "thread_spindle"
        check_thread_running(robot,name, action, action_input)
        thread = ExecuteActionThread(8, name, robot, action, action_input)

    elif self.action == "navigate_generic":
        name ="thread_navigate"
        check_thread_running(robot,name, action, action_input)
        thread = ExecuteActionThread(9, name, robot, action, action_input)

    elif self.action == "perception_recognition":
        name ="thread_perception"
        check_thread_running(robot,name, action, action_input)
        thread = ExecuteActionThread(10, name, robot, action, action_input)   

    self.reasoner.assertz(Compound("thread_ready_for_execution", name, action, action_input))

    return thread

def check_thread_running(self, name, action, action_input):
    self.robot = self

    thread_running = self.robot.reasoner.query(Compound("thread_running", name, action, action_input))

    if thread_running:
        thread_runs = True
        while thread_runs:
            thread_running = self.robot.reasoner.query(Compound("thread_running", name, action, action_input))
            if thread_running:
                rospy.loginfo("[AG] Thread {0} is already busy.".format(name))
                rospy.sleep(1)
            else:
                rospy.loginfo("[AG] Thread {0} has finished.".format(name))
                thread_runs = False
    else:
        rospy.loginfo("[AG] Thread is not busy.")

    return


class ExecuteActionThread (threading.Thread):
    def __init__(self, threadID, name, robot, action, action_input):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.robot = robot
        self.action = action
        self.action_input = action_input

    def stop(self):
        self.__stop = True

    def run(self):

        rospy.loginfo("[AG] Starting {0}".format(self.name))

        self.robot.reasoner.query(Compound("retractall", Compound("thread_ready_for_execution", self.name, self.action, self.action_input)))    
        self.robot.reasoner.assertz(Compound("thread_running", self.name, self.action, self.action_input))

        result = execute(self.robot, self.action, self.action_input)      

        if result == "success":
            rospy.loginfo("[AG] Asserting success for {0}".format(self.name))
            #rospy.loginfo("[AG] TEST Action success = {0}".format(self.action))
            #rospy.loginfo("[AG] TEST Action_input success = {0}".format(self.action_input))
            robot.reasoner.query(Compound("assert_done", "success", self.action, self.action_input))
            self.robot.reasoner.assertz(Compound("thread_finished_succes", self.name, self.action, self.action_input))
        if result == "failed":
            rospy.loginfo("[AG] Asserting failed for {0}".format(self.name))
            #rospy.loginfo("[AG] TEST Action failed = {0}".format(self.action))
            #rospy.loginfo("[AG] TEST Action_input failed = {0}".format(self.action_input))
            robot.reasoner.query(Compound("assert_done", "failed", self.action, self.action_input))
            self.robot.reasoner.assertz(Compound("thread_finished_failed", self.name, self.action, self.action_input))

        rospy.loginfo("[AG] Exiting {0}".format(self.name))
        
        rospy.loginfo("[AG] thread_running is retracted:")
        self.robot.reasoner.query(Compound("retractall", Compound("thread_running", self.name, self.action, self.action_input)))   


if __name__ == "__main__":

    rospy.init_node('atomic_grounding_executioner')
    rospy.loginfo("[AG = Atomic Grounding]")

    robot = startup()
    initialize(robot)
    # add initial pose here
    init_retract_facts(robot)

    rospy.loginfo("[AG] Initialized")

    robot.reasoner.query(Compound("indigolog","clean_up_challenge"))

    # Create new threads
    main_thread = GetActionThread(1, "Main_thread-1", robot)

    # Start Main Thread
    main_thread.start()

    while not rospy.is_shutdown():
        #thread_list = threading.enumerate()
        #rospy.loginfo("[AG] thread list: {0}".format(thread_list))
        rospy.sleep(1)

    main_thread.stop()
