#!/bin/bash

# Keep old terminated containers in check by keeping only the latest N terminated containers

latest_containers_to_keep=5

# Ensure that only one copy of the script is running
/usr/bin/lockfile -r 0 /tmp/clean_old_containers.lock 2>/dev/null
result=$?
if [ $result != 0 ]; then
  exit 0
fi

# Delete the old terminated containers
for image_name in $(docker ps -a -f status=exited -f status=dead -f status=created | awk '{print $2}' | sort | uniq  | egrep -v "amazon-ecs-agent|ID"); do
    for old_image in $(docker ps -a -f status=exited -f status=dead -f status=created | grep $image_name | awk '{print $1}' | tail -n+${latest_containers_to_keep}); do
        docker rm ${old_image}
    done
done

# Clean up the lockfile
rm -f /tmp/clean_old_containers.lock
