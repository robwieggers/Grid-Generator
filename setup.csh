#! /bin/tcsh -f
echo "setting up the system"

echo "selected compiler settings:" $OBJECTCODE
echo "in case you want to switch compiler settings, modify ./setup/setup.csh"
setenv TOPDIR $PWD 
setenv LIBDIR ${TOPDIR}/lib
setenv BINDIR ${TOPDIR}/bin
setenv MODDIR ${TOPDIR}/modules

setenv OBJECTCODE linux.gfortran
#setenv OBJECTCODE linux.g95
#setenv OBJECTCODE linux.ifort

echo " "
echo "################################################"
echo "#   installing libraries                       #"
echo "################################################"
echo "installing json-fortran"
./json-fortran_build gfortran

echo " "
echo "################################################"
echo "#   installing external tools                  #"
echo "################################################"

echo "installing triangle and showme"
./triangle_build

#set jsonFortranPath =  `locate libjsonfortran.so | grep lib | head -1`
#set jsonFortranDir = `dirname $jsonFortranPath`
#setenv JSONFORTRANDIR $jsonFortranDir

echo " "
echo "################################################"
echo "#   done                                       #"
echo "################################################"
