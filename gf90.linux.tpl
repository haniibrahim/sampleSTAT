# Linux template
# gfortran template file for mkmf
# mkmf -p sampleSTAT -t gf90.linux.tpl -m makefile.linux.mak
# make -f makefile.linux.mak

FC = gfortran
LD = gfortran
FFLAGS = -s -O3
LDFLAGS = -static

