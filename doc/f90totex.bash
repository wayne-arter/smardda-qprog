for i in bigobj_h.f90 bigobj_m.f90 qcontrol_h.f90 qcontrol_m.f90 qprog.f90 oo_m.f90 template_m.f90;
do rm -f $i.tex ;
cat ../sv.tex ../$i ../vn.tex > $i.tex ;
done
