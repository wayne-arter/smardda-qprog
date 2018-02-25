#! /bin/bash
## Script for object-oriented Fortran program
# Example where
# set program, program object and one other object
# contents of qprog.txt used to generate namelist
# contents of bigobj.txt merely define data structure
QPROG=xpc
Q=xp
STR="transport_coefficients"
BIGOBJ=clcoef
BO=cc
BSTR="classical_coefficients"
## set variables from list key=arg
for i in $*; do
key=${i%%=*}
arg=${i##*=}
case "$key" in
  QPROG) QPROG=${arg//+/ };;
  Q) Q=${arg//+/ };;
  STR) STR=${arg//+/ };;
  BIGOBJ) BIGOBJ=${arg//+/ };;
  BO) BO=${arg//+/ };;
  BSTR) BSTR=${arg//+/ };;
  ?) echo "unknown key";exit;;
esac
done
# remove underlines from strings
STR=${STR//_/ }
BSTR=${BSTR//_/ }
#Logging
echo "program name = " $QPROG
echo "program id = " $Q
echo "program description = " $STR
echo "object name = " $BIGOBJ
echo "object id = " $BO
echo "object description = " $BSTR
# other initialisation
export SM=${SMITER_DIR%/*}
export SMPROG=${SMITER_DIR%/*}/qprog
export SMDEV=${SMITER_DIR%/*}/develop
#
rm -f spaced.txt work.txt setvar.txt namvarinit.txt namvars.txt namvardecl.txt include.txt
if [ ! -e $QPROG.txt ] ; then cp $SMPROG/qprog.txt $QPROG.txt;fi
if [ ! -e $BIGOBJ.txt ] ; then cp $SMPROG/bigobj.txt $BIGOBJ.txt;fi
## process variables
#ensure program var file spaced OK
sed -e "s/ *= */ = /" -e "s/ *:: */ :: /" -e 's/ *!< */ !< /' \
-e "s/^ */  /" < $QPROG.txt  > spaced.txt
sed \
-e "s/ /_/g" < spaced.txt | sed -e "s/^__/  /" -e "s/_::_/ :: /" \
| sed -e 's/_!<_/ !< /' | sed -e "s/_=_/ = /" > work.txt
awk '{print "     " $1 " " $2 " " $3 " " $6 " " $7}' < work.txt > include.txt
awk '{print "  " $1 " " $2 " " $7 " " $6 " self-explanatory local"}' < work.txt > namvardecl.txt
awk '{print " & " $7 " , " "&" }' < work.txt > namvars.txt
awk '{print "  " $7 " " $4 " " $5 }' < work.txt >namvarinit.txt
awk '{print "  selfn%" $3 " " $4 " " $7 }' < work.txt > setvar.txt
#space object var file spaced OK
sed -e "s/ *= */ = /" -e "s/ *:: */ :: /" -e 's/ *!< */ !< /' \
-e "s/^ */     /" < $BIGOBJ.txt  > spaced.txt
## process f90 files
# qprog main file (note qprog is a bigobj)
sed \
-e "s/qprog/$QPROG/g" \
-e "13a\  use "$BIGOBJ"_h" \
-e "13a\  use "$BIGOBJ"_m" \
-e "s/qcontrol_read(file,param,bigobj%n,/qcontrol_read(file,param,bigobj%n,bigobj%"$BIGOBJ"%n,/" \
-e "s/qcontrol_/"$Q"control_/g" \
-e "s/qfiles/"$Q"files/g" \
-e "s/qplots/"$Q"plots/g" \
-e "s/qparams/"$Q"params/g" \
-e "s/bigobj/$QPROG/g" \
-e "s/one-line program description/$STR/" \
< $SMPROG/qprog.f90 > $QPROG.f90
# control include file
sed \
-e "s/qprog/$QPROG/g" \
-e "s/qcontrol_/"$Q"control_/g" \
-e "s/qfiles/"$Q"files/g" \
-e "s/qplots/"$Q"plots/g" \
-e "s/qparams/"$Q"params/g" \
-e "s/bigobj/$QPROG/g" \
< $SMPROG/qcontrol_h.f90 > "$Q"control_h.f90
# control source file
sed \
-e "s/qprog/$QPROG/g" \
-e "7a\  use "$BIGOBJ"_h" \
-e "7a\  use "$BIGOBJ"_m" \
-e "80a\  type("$BO"numerics_t), intent(out) :: "$BO"numerics !< controls for $BSTR" \
-e "199a\  call "$QPROG"_readcon("$Q"numerics,nin)" \
-e "199a\  " \
-e "199a\  call "$BIGOBJ"_readcon("$BO"numerics,nin)" \
-e "s/qcontrol_read(file,param,bonumerics,/qcontrol_read(file,param,bonumerics,"$BO"numerics,/" \
-e "s/qcontrol_/"$Q"control_/g" \
-e "s/qfiles/"$Q"files/g" \
-e "s/qplots/"$Q"plots/g" \
-e "s/bonumerics/"$Q"numerics/g" \
-e "s/qparams/"$Q"params/g" \
-e "s/bigobj/$QPROG/g" \
< $SMPROG/qcontrol_m.f90 > "$Q"control_m.f90
# program source file
sed \
-e "s/bonumerics/"$Q"numerics/g" \
-e "s/noutbo/nout$Q/g" \
-e "s/ninbo/nin$Q/g" \
-e "s/bigobj/$QPROG/g" \
-e "6a\  use "$BIGOBJ"_h" \
-e "6a\  use "$BIGOBJ"_m" \
-e "183r setvar.txt" \
-e "112r namvarinit.txt" \
-e "100r namvars.txt" \
-e "92r namvardecl.txt" \
< $SMPROG/bigobj_m.f90 > "$QPROG"_m.f90
#program include file
sed \
-e "s/bonumerics/"$Q"numerics/g" \
-e "s/bigobj/$QPROG/g" \
-e "21a\  type("$BIGOBJ"_t) :: $BIGOBJ !< $BSTR" \
-e"/^! $Note/d" \
-e "9r include.txt" \
-e "3a\  use "$BIGOBJ"_h" \
< $SMPROG/bigobj_h.f90 > "$QPROG"_h.f90
# object source file
sed \
-e "s/bonumerics/"$BO"numerics/g" \
-e "s/noutbo/nout$BO/g" \
-e "s/ninbo/nin$BO/g" \
-e "s/bigobj/$BIGOBJ/g" \
< $SMPROG/bigobj_m.f90 > "$BIGOBJ"_m.f90
# object include file
sed \
-e "s/bonumerics/"$BO"numerics/g" \
-e "s/bigobj/$BIGOBJ/g" \
-e "20r spaced.txt" \
-e"/^! $Note/d" \
< $SMPROG/bigobj_h.f90 > "$BIGOBJ"_h.f90
##process ctl file and set links
# input file
sed \
-e "s/qprog/$QPROG/g" \
-e "s/bigobj/$BIGOBJ/g" \
< $SMPROG/qprog.ctl > $QPROG.ctl
#
ln -sf $SMITER_DIR/f95/const_kind_m.f90
ln -sf $SMITER_DIR/f95/const_numphys_h.f90
ln -sf $SMITER_DIR/f95/date_time_m.f90
ln -sf $SMITER_DIR/f95/log_m.f90
ln -sf $SMITER_DIR/f95/clock_m.f90
ln -sf $SMITER_DIR/f95/gfile_m.f90
ln -sf $SMITER_DIR/f95/vfile_m.f90
ln -sf $SMITER_DIR/fortd LIB
(cd ..;ln -sf $SMITER_DIR/config)
#lastly produce Makefile and run program
$SMDEV/makemake $QPROG
mv Makefile.1 Makefile.$QPROG
make -f Makefile.$QPROG
./$QPROG $QPROG
