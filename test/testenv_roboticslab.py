#!/usr/bin/python
import roslib; roslib.load_manifest('fast_simulator')
import rospy

from fast_simulator import client

if __name__ == "__main__":
    rospy.init_node('open_challenge_rgo2013_tester')

    W = client.SimWorld()

    # table_x = 2.683
    # table_y = -1.637

    # table = W.add_object("table-1", "table", table_x, table_y, 0)
    W.add_object("seven_up-1",  "seven_up",  2,-1, 0.88)
    W.add_object("coke-1",  "coke",  1,2,0.88)
    #W.add_object("person-1", "person", 6.309, -1.0, 0)
    W.add_object("person-1", "person", 8.309, -3.0, 0)

