#!/bin/sh

backupdirs="/home/admin/docker-apps"
host=pooper #the name of the host to use in google cloud
bucket=nbren12-backups

cd /home/admin

mkdir -p backups
datestr=`date +%Y-%m-%d-%H`
tarball=backups/webapp-$datestr.tar.gz


echo "noahbrenowitz.com backup program"
echo "--------------------------------"
date
echo "Archiving webapp/ to $tarball"
echo ""

sudo tar czf $tarball $backupdirs

echo "Copying file to gs://nbren12-backups/$host/"
gsutil cp $tarball gs://nbren12-backups/$host/

echo "Backup complete...."
echo "-------------------------------"
