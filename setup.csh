#! /bin/tcsh -f
echo "setting up the system"

setenv OBJECTCODE linux.gfortran
#setenv OBJECTCODE linux.g95
#setenv OBJECTCODE linux.ifort

echo "selected compiler settings:" $OBJECTCODE
echo "in case you want to switch compiler settings, modify ./setup/setup.csh"
setenv TOPDIR $PWD 
