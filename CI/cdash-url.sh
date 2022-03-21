#!/bin/bash

dir=$1
cdash_url=$2
tag=$(head -1 $dir/TAG)
Done=$(cat $dir/$tag/Done.xml)
buildID=$(echo $Done | grep -o -P '(?<=buildId>).*(?=</build)')
URL=$cdash_url/viewTest.php?buildid=$buildID
echo $URL
