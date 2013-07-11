#!/usr/bin/python
import roslib; roslib.load_manifest('fast_simulator')
import rospy

from fast_simulator import client

if __name__ == "__main__":
    rospy.init_node('open_challenge_rgo2013_tester')

    W = client.SimWorld()

    W.add_object("seven_up-1",  "seven_up",  2,-1, 0.88)
    W.add_object("coke-1",  "coke",  0.806845715361795,2.1867359796956563,0.5493168002464702)
    W.add_object("coke-2",  "coke",  1.5, -1, 0.9)
    W.add_object("person-1", "person", 8.309, -3.0, 0)

