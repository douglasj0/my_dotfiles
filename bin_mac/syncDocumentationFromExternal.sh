#!/bin/bash
#
# -a archive (equiv to -rlptgoD)
# -v verbose
# -u update
# -z compress

# rsync -avz --delete /Users/jacksond/Public/Documentation/ /Users/Shared/Documentation
# rsync -avz /Volumes/Wanderer/Documentation/ /Users/jacksond/Public/Documentation

rsync -avz /Volumes/Wanderer/Documentation/ /Users/Shared/Documentation