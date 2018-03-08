#!/bin/env python

# Control an SSH based tun VPN

# Copyright (c) 2007 Ryan McGuire http://www.enigmacurry.com
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# Put this file somewhere on your path and rename it to just "vpn-up"
# Also make a symlink to it called "vpn-down".
#
# This has an example connection called dissident. You'll want to use that line
# as a model for setting up your own connection. The format is the following:
#
# connections.append(
#    Connection('Descriptive Name','remote hostname',
#               'Local IP for tun0','Remote IP for tun0',
#               'The base Network IP (eg 10.0.0.0)','netmask (eg 255.255.255.0)'))
#
# Usage is simple:
#   vpn-up dissident      -- Establishes a VPN for the dissident connection
#   vpn-down dissident    -- Turns off the VPN for the dissident connection
#
# Just replace 'dissident' with your connection name


import sys
import os
import time
import subprocess
import signal
import shlex

connections = []

class Connection:
    def __init__(self, name, remote_host, local_ip, remote_ip,
                 network, netmask, local_tun=0, remote_tun=0):
        self.name = name
        self.remote_host = remote_host
        self.local_ip = local_ip
        self.remote_ip = remote_ip
        self.network = network
        self.netmask = netmask
        self.local_tun = local_tun
        self.remote_tun = remote_tun
        self.ssh_proc = None
        self.remote_device = "eth0"
    def up(self):
        """Bring the connection up"""
        print "Connecting %s VPN..." % self.name
        cmd = "ssh -f -w %(local_tun)s:%(remote_tun)s %(remote_host)s 'ifconfig tun%(remote_tun)d %(remote_ip)s pointopoint %(local_ip)s; arp -sD %(local_ip)s %(remote_device)s pub'" % self.__dict__
        self.ssh_proc = subprocess.Popen(
            shlex.split(cmd),stdin=sys.stdin,stdout=sys.stdout)
        self.ssh_proc.wait()
        time.sleep(5)
        os.system("ifconfig tun0 %s pointopoint %s" % \
                  (self.local_ip,self.remote_ip))
        os.system("route add -net %s netmask %s gw %s tun%d" % \
                  (self.network,self.netmask,self.remote_ip,self.local_tun))
    def down(self):
        """Bring the connection down"""
        #We can't keep track of the pid because
        #ssh -f spawns a new PID after connecting
        #Attempt to make a really good guess at the pid
        ssh_grep = subprocess.Popen(
            ['ps','-ef'],stdout=subprocess.PIPE)
        ps_output = ssh_grep.communicate()[0].split("\n")
        for line in ps_output:
            if "ssh" in line and self.remote_host in line and \
                   "tun%d" % self.local_tun in line:
                #Find the pid
                pid = int(line.split()[1])
                os.kill(pid,signal.SIGTERM)
                break
        else:
            print "Can't find a PID for the VPN process. Try killing manually"
            

connections.append(
    Connection('dissident','dissident.enigmacurry.com','192.168.200.11',
               '192.168.200.10','192.168.200.0','255.255.255.0'))


if __name__ == "__main__":
    if os.getuid() != 0:
        print "You must run this as root."
        sys.exit(1)
    if len(sys.argv) != 2:
        print "Usage:"
        print "  vpn-up connection-name"
        print "  vpn-down connection-name"
        sys.exit(1)
    if sys.argv[0].endswith("vpn-up"):
        mode="up"
    elif sys.argv[0].endswith("vpn-down"):
        mode="down"
    else:
        print "Unknown mode for command:",sys.argv[0]
        sys.exit(1)
    connection = sys.argv[1]
    for conn in connections:
        if conn.name == connection:
            #bring connection up/down
            if mode == "up":
                conn.up()
            elif mode == "down":
                conn.down()
            break
    else:
        print "Unknown connection:",connection
        sys.exit(1)
                
            
