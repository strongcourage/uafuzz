#!/bin/sh
#
# check_tarball.sh
#
# This small script tests that at the very least the BINSEC distribution builds
# correctly. 
#
# It builds a tarball, untar it (with both GNU & BSD tar), and goes through the
# compilation process
#
# It is very useful to test BSD tar since it is the one used on Mac OS X 
# It behaves slightly differently than GNU tar so it is best to test it.
#


TDIR="distrib-test-directory"
set +x



function check () {
    mytar=$1
    echo "Checking with $mytar ..."
    rm -Rf  ${TDIR}
    # The following line only works at BINSEC's toplevel
    make -f Distrib.mk tarball
    mkdir -p ${TDIR}
    TGZ=$(ls binsec-*.tgz)
    echo "Will untar $TGZ"
    V=$(basename $TGZ .tgz)
    (cd ${TDIR};
     ${mytar} xvzf ../${TGZ};
     cd ${V};
     autoconf;
     ./configure;
     make binsec;
     make pinsec)
}


check "tar"
check "bsdtar"


# Cleanup in the end if everything went well
rm -Rf ${TDIR}
