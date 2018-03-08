#!/bin/bash
#
# -a archive (equiv to -rlptgoD)
# -v verbose
# -u update
# -z compress

# rsync -avz --delete /Users/Shared/Documentation/ /Volumes/Wanderer/Documentation
# rsync -auvz --delete --progress /Users/jacksond/Public/Documentation/ /Volumes/Wanderer/Documentation

rsync -avz /Users/Shared/Documentation/ /Volumes/Wanderer/Documentation
