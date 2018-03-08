#!/bin/sh
ls *m4a | cut -d . -f1 | awk '{print "mv ./\""$0".m4a\" ./\""$0".m4b\""}' | bash
