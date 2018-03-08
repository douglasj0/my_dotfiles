#!/bin/sh

svn log --revision HEAD:1 --limit 10 --verbose https://shrpzsrc01/repos/OrbitGateway/orbit_gateway_web | less
