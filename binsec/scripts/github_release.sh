#!/bin/sh

# Summarize and automate the steps to release on Github

function pp () {
    echo "$1 ..."
}

local_dir=`pwd`
github_release_dir=${GHUB_DIR:-${local_dir}/../github-releases}

pp "Cd'ing $github_release_dir"
cd ${github_release_dir}

remote_dir=`pwd`

if [ "$remote_dir" = "$local_dir" ]; then
    pp "Remote and local directory are the same. Aborting"
    exit 1
fi

pp "Removing *all* files from git directory"
git clean -f
files=`git ls-files`
rm -f ${files}

dirs=`ls -d */`
if [ $? -eq 0 ]; then
    pp "Removing directories"
    rm -Rf ${dirs}
else 
    pp "No directories found. Skipping directories removal"
fi

cd ${local_dir}
pp "Making tarball from ${local_dir}"
make -f Distrib.mk tarball
tarball=`ls *.tgz`

# The tarball is assumed to follow the following format
# binsec-<version>-<date>.tgz
# Let's extract the version name
version=`basename $tarball .tgz | cut -d '-' -f 2` 

pp "Extracting"
tar xvzf $tarball --strip-components=1 -C ${github_release_dir}

cd ${github_release_dir}
pp "Adding new files in git directory"
git add .

git commit -am "Release BINSEC ${version}"
tag="binsec-$version"
git tag -f $tag


msg="Now run git push origin master; \
         git push origin $tag to push to github"
say=`which cowsay`
if [ $? -ne 0 ]; then
    echo ""
    echo "****************************************************"
    echo "* $msg *"
    echo "****************************************************"
    echo ""
else
    echo ""
    cowsay $msg
fi
