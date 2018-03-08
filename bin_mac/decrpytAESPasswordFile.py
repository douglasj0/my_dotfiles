#!/usr/bin/python

import os
import sys
import argparse

parser = argparse.ArgumentParser(description='Decrypt a file')
parser.add_argument('-n', '--name', action='store', dest='file_name',
        help='The name or absolute path of the file', required=True)
options = parser.parse_args()

user_home = os.path.expanduser('~')
key_file = user_home + "/.aescrypt_key"

aescrypt_path = user_home + "/dev/aescrypt"
sys.path.append(aescrypt_path) 
from aescrypt import AesCrypt
cryptObj = AesCrypt(key_file=key_file)

encrypted_pw = [line.strip() for line in open(options.file_name)]
password = cryptObj.decrypt(encrypted_pw[0])

print password
