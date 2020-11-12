#!/bin/sh
#DIRS="thirdparty/docopt.fs/Build jrutil jrutil-multitool jrunify htmltt georeport test"
DIRS="thirdparty/docopt.fs/Build jrutil jrutil-multitool jrunify htmltt georeport rtcollect"
for dir in $DIRS; do
	rm -rf $dir/bin $dir/obj
done
for dir in $DIRS; do
	dotnet build $dir
done
