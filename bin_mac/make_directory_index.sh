#!/bin/bash
# made by quite, idea by ioccatflashdancedotcx :P

for i in * ; do \ls -ldG "$i" | sed 's|^\(.* [0-9][0-9]:[0-9][0-9]\) \(.*\)$|\1 <a href="\2">\2</a>|g' ; done >> index.html
