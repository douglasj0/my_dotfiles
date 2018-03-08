#!/usr/bin/env python
# This script reads the saved contents of an agenda buffer created by
# org-mode then creates a reformatted, compact version to standard
# output, showing lists by context for each day of the agenda.

# Author: Charles Cave     (charlesweb@optusnet.com.au)
# Date:   7th February 2009

# This is free software.  You can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

import re

f = open("agen.txt", "r")
prevtag = "XYXYX"           # a non-existent tag

for line in f:
    line = line[:-1]        # remove newline from end of line
    if line[:4] == "Week":
        print line
        prevtag = "XYXYX"
        continue
    if line[:1] != " ":
        print "\n\n\n-------------------------------------------------"
        print line
        continue
    if line[14:16] == "In":    
        continue
    item = line[26:]
    timelabel = line[14:25]
    if timelabel == "Scheduled: ":
        timelabel = ""
    if timelabel == "Deadline:  ":
        continue   # not interested in printing Deadlines

    todo = re.search('^TODO (.*?)$', item)
    if todo:
        item = todo.group(1)

    thistag = ""
    tagged = re.search('^(.*?)\s*:(.*?):', item)
    if tagged:
        item = tagged.group(1)
        thistag = tagged.group(2)

    if thistag != prevtag:
        print "\n\n", thistag
        prevtag = thistag
    print "[ ] %s %s"  %  (timelabel, item)
