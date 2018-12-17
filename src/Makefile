# add any other compilation flag
FFLAGS =  -ffree-line-length-none
LDFLAGS = 
FG = gfortran

main.exe: dynamic.f90 bspline_sub_module.o bspline_oo_module.o bspline_module.o distrib.o misc.o interp.o dynlib.o
	$(FG) $^ $(LDFLAGS) -o $@

%.o: %.f90
	$(FG) $(FFLAGS) -c $<

.PHONY: clean

clean:
	rm -rf *.o *.mod main.exe
