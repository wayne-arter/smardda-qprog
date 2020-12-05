# config file.
include ../config/config.inc

src =  isnumb.f lowor.f

obj = $(src:.f=.o)

libsmarddalib$(DBG).a: $(obj) 
	ar ru libsmarddalib$(DBG).a $(obj) 
	ranlib libsmarddalib$(DBG).a

tar: $(src) README makefile
	tar cvf fort.tar $(src)  README makefile

clean: 
	rm -f $(obj) 

libclean: clean
	rm -f libsmarddalib$(DBG).a
