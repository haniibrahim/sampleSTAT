# Makefile created by mkmf $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 



include gf90.tpl


.DEFAULT:
	-echo $@ does not exist.
all: sampleSTAT
dbg_mdl.o: ./dbg_mdl.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./dbg_mdl.F90
readdata_mdl.o: ./readdata_mdl.f90 sysconst_mdl.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./readdata_mdl.f90
sampleSTAT.o: ./sampleSTAT.f90 sysconst_mdl.o samplestatistics_mdl.o sngall_mdl.o readdata_mdl.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./sampleSTAT.f90
samplestatistics_mdl.o: ./samplestatistics_mdl.f90 sysconst_mdl.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./samplestatistics_mdl.f90
sng_mdl.o: ./sng_mdl.F90 sysconst_mdl.o dbg_mdl.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./sng_mdl.F90
sngall_mdl.o: ./sngall_mdl.F90 dbg_mdl.o sng_mdl.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./sngall_mdl.F90
sysconst_mdl.o: ./sysconst_mdl.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./sysconst_mdl.f90
SRC = ./dbg_mdl.F90 ./readdata_mdl.f90 ./sngall_mdl.F90 ./sysconst_mdl.f90 ./samplestatistics_mdl.f90 ./sng_mdl.F90 ./sampleSTAT.f90
OBJ = dbg_mdl.o readdata_mdl.o sngall_mdl.o sysconst_mdl.o samplestatistics_mdl.o sng_mdl.o sampleSTAT.o
clean: neat
	-rm -f .sampleSTAT.cppdefs $(OBJ) sampleSTAT
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
sampleSTAT: $(OBJ) 
	$(LD) $(OBJ) -o sampleSTAT  $(LDFLAGS)
install:
	cp sampleSTAT /usr/local/bin/
	cp sampleSTAT.1 /usr/local/man/man1/
uninstall:
	rm -f /usr/local/bin/sampleSTAT
	rm -f /usr/local/man/man1/sampleSTAT.1
