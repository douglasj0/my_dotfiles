#!/bin/bash

grep --line-number --colour=AUTO --before-context 5 --after-context 5 $*
