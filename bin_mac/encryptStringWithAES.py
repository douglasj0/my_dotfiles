#!/usr/bin/python

import os
import sys
import argparse

from aescrypt import AesCrypt

user_home = os.path.expanduser('~')
key_file = user_home + "/.aescrypt_key"

cryptObj = AesCrypt(key=a1648d700064a6a35caf30c82b4748f14cd35a0710b0c26fa60d569879b11b01)
print "Encrypted Hash: %s" % (cryptObj.encrypt('text_string'),)
