#!/bin/bash

aws --profile preprod autoscaling describe-auto-scaling-groups --region us-east-1 --output json | jq '.AutoScalingGroups[] | select(.MinSize == 0 and .MaxSize == 0 and .DesiredCapacity == 0).Tags[0].ResourceId' | sed -e 's/\"//g' #> asgs
