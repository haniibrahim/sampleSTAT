# Makefile created by mkmf $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 



include gf90.mac.tpl


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
sysconst_mdl.o: ./sysconst_mdl.F90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./sysconst_mdl.F90
SRC = ./readdata_mdl.f90 ./samplestatistics_mdl.f90 ./sysconst_mdl.F90 ./sampleSTAT.f90 ./sngall_mdl.F90 ./sng_mdl.F90 ./dbg_mdl.F90
OBJ = readdata_mdl.o samplestatistics_mdl.o sysconst_mdl.o sampleSTAT.o sngall_mdl.o sng_mdl.o dbg_mdl.o
clean: neat
	-rm -f .sampleSTAT.cppdefs $(OBJ) sampleSTAT
	-rm -f *.mod
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
sampleSTAT: $(OBJ) 
	$(LD) $(OBJ) -o sampleSTAT  $(LDFLAGS)
install:
	sudo cp ./sampleSTAT /usr/local/bin/
	sudo cp ./sampleSTAT.1 /usr/local/share/man/man1/
