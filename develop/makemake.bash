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
if [ $sw == local ] ; then
  SMDEV=$(pwd)/develop
  PLUS=plus
else
## HS set to smiter top directory
  if [[ -z "$HS" ]] ; then
    if [[ -n "$SMITER_DIR" ]] ; then HS=$SMITER_DIR; else HS=$HOME/smardda/smiter; fi
  fi
  SMDEV=${HS%/*}/develop
  if [ ! -d $SMDEV ] ; then
    echo "Directory \$SMDEV=$SMDEV does not exist - installation corrupt ?"
    echo "Script quitting"; exit
  fi
  PLUS=
fi
PROG=${1%.f90}
grep "^  *use" $PROG.f90 > filelist
ex filelist << @@
g/^ *use /s/ *$/.f90/
g/^ *use /s/^ *use //
%j
wq
@@
grep "^  *use" $PROG.f90 $(cat filelist) > uselist
ex uselist << @@
g/: *use /s/: *use / : /
g/^/s/ *$/.mod/
wq
@@
grep "^  *use" $PROG.f90 > sourcelist
echo  "$PROG.f90" >> sourcelist
ex sourcelist << ++
g/^ *use/s:$:.f90 \\\:
g/^ *use /s/^ *use //
wq
++
echo " " >> sourcelist
echo "PROG = $PROG" >> sourcelist
rm -f Makefile.1
cat $SMDEV/Makefile.hed sourcelist $SMDEV/Makefile.mid$PLUS uselist > Makefile.1
rm -f uselist sourcelist
