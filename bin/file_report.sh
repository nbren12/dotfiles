#!/bin/sh

report=$1

target=vcm-ml-public/noah/reports/$(date +%Y)/$(date +%F)-$report

echo "uploading report to" 
echo $target

gsutil cp $report gs://${target}
echo
echo
echo "**************************************************"
echo "Permanent link available here:"
echo https://storage.googleapis.com/$target
echo "**************************************************"

