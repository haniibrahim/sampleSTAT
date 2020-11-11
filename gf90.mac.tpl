# MAC template
# gfortran template file for mkmf
# mkmf -p sampleSTAT -t gf90.mac.tpl -m makefile.mac

FC = gfortran
LD = gfortran
FFLAGS = -s -O3
LDFLAGS = -static-libgfortran -static-libgcc /opt/local/lib/gcc9/libquadmath.a