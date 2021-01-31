#! /bin/bash
# Generate makefile from source code, file Makefile.1
# -l option to work as far as possible with files in directory
sw=global
if [ $# -gt 0 ] ;  then
  test=$1
  if [ ${test#-} != $test ] ; then
    if [ ${1#-} = l ] ;  then sw=local ; fi
    shift
  fi
fi
# Main program must reference i.e. "use" all modules used
if [ $# -eq 0 ] || [ $# -gt 2 ] ; then
  echo "Usage: $0 [-l] [<root of code name>]"
  exit
fi
SMDEV=$(pwd)/develop
if [ $sw == local ] ; then
  PLUS=plus
  VPATH=
else
  PLUS=
  VPATH=path
fi
PROG=${1%.f90}
grep "^  *use" $PROG.f90 > filelist
ex filelist << @@
g/: *use /s/ *!.*$//
g/^ *use /s/ *$/.f90/
g/^ *use /s/^ *use //
%j
wq
@@
grep -s "^  *use" $PROG.f90 $(cat filelist) > uselist
ex uselist << @@
g/: *use /s/ *!.*$//
g/: *use /s/: *use / : /
g/^/s/ *$/.mod/
wq
@@
grep "^  *use" $PROG.f90 > sourcelist.0
ex sourcelist.0 << ++
g/: *use /s/ *!.*$//
g/^ *use/s:$:.f90:
g/^ *use /s/^ *use //
wq
++
rm -f sourcelist
for i in $(cat sourcelist.0); do if [[ -e $i ]] ; then echo "$i \\">> sourcelist ;fi;done
echo  "$PROG.f90" >> sourcelist
echo " " >> sourcelist
echo "PROG = $PROG" >> sourcelist
rm -f Makefile.0 Makefile.1
cat $SMDEV/Makefile.hed$VPATH sourcelist $SMDEV/Makefile.mid$PLUS uselist > Makefile.0
#fix up for mpi work side-effects
sed -e "s/LIB\/lib/LIB\/libsmarddabit/" \
-e "s/ mpi.mod//" \
< Makefile.0 > Makefile.1
rm -f filelist uselist sourcelist sourcelist.0 Makefile.0
