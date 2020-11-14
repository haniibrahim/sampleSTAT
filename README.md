# sampleSTAT - Statistics for Sampling Distributions #

**sampleSTAT** is a command-line application to determine sampling distributions. It goes a lot further than arithmetic mean and standard deviation. It uses floating point variables according [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point) which make results portable (see Chap. Accuracy). sampleSTAT is written in Fortran 90.

sampleSTAT is based on the German book *R. Kaiser, G. Gottschalk; "Elementare Tests zur Beurteilung von Meßdaten", BI Hochschultaschenbücher, Bd. 774, Mannheim 1972.*

## Purpose ##
Performs statistical tests for sampling distributions:
- Arithmetic Mean
- Sample Standard Deviation
- Range of Dispersion of Values
- Range of Dispersion of Mean
- Minimum Value
- Maximum Value

### Arithmetic Mean (x) ###
The arithmetic mean is the best and significant approach to specify an result when it will used numerically. For its alone it does not say anything about the quality of the result. An artithmetic mean of 10 can be generated from both examples below:

| Sample 1  |  Sample 2 |
| --------: | --------: |
|         6 |     9.999 |
|         8 |     9.998 |
|        14 |    10.002 |
|        12 |    10.000 |
|         5 |    10.001 |
|        15 |    10.000 |

### Number of Values (N) ###

The amount of single values (N) is a important degree to evaluate the security of the result. N specify the measuring expenditure but N itself has no significance to the the result. In both examples above N was the same.

### Sample Standard Deviation (s) ###

The standard deviation is the most informative quantity to specify the statistic quality of a measuring procedure. With a probability of 68% the *values* will stray with +/- s around the mean.

But *s* itself does not say anything about the quality of the result but in combination with *n* you can calculate the range of dispersion.

### Range of Dispersion of Values (T) ###
The range of dispersion of the values is calculated as `T = s * t`. t is the student factor, dependent on the statistic security/confidence level P% (95%, 99%, 99.9%) and the degree of freedom `f = n - 1`. *f* and *t* are specified in the t-tables.

*T* indicates that P% of all single *values* - with x as its mean - are expected around `x + T` and `x - T`. *T* is the parameter which specify the quality of the raw values.

### Range of Dispersion of Mean ###

This parameter tells how secure the mean is. It indicates the stray area of the mean and not of the raw values as *T* does. The *mean* will stray with a probability of P% around the mean.

See chapter "Example" for more details.

## Accuracy ##

It uses [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point) *(IEEE Standard for Binary Floating-Point Arithmetic for microprocessor systems (ANSI/IEEE Std 754-1985))* real values which give the same results independently on which architecture (32 or 64-bit) the sources were compiled and run.

## Usage ##

Open a terminal on UNIX or CMD.EXE on Windows® and type `sampleSTAT --help` for help:

```
sampleSTAT performs tests for statistical samples:
   Arithmetic Mean, Range of Dispersion of values and mean based on t-factor,
   Standard Deviation, Minimum, Maximum.

Usage: sampleSTAT [-hv] [-l P] [<inputfile] [>outputfile]
  -h    --help /?   Print this help screen
  -v    --version   Print version information
  -l P  --level=P   Set confidence level:
                    P=95   conf. level: 95%
                    P=99   conf. level: 99%
                    P=99.9 conf. level: 99.9%

Examples:
  sampleSTAT -l 95 <mydata.dat
  sampleSTAT --level=99 <mydata.dat >results.txt
  sampleSTAT -l 99.9 <mydata.dat

Input data:
  Data has to be committed in a one column form, like:
     22.43
     22.45
     22.50

```

or type `man sampleSTAT` on UNIX(-like) systems or macOS after installation. On Windows refer `sampleSTAT.html` or `README.md` which are in the (binary) distribution instead.

### Examples

sample1.dat:
```
6
8
14
12
5
15
```

```
$ sampleSTAT -l 0 <sample1.dat

 sampleSTAT - Statistics for Sampling Distributions
 ==================================================
 Number of Values            :    6
 Arithmetic Mean             :    10.000000000000000
 Confidence Level            :    95%
 Range of Dispersion (values):    10.903586565896561
 Range of Dispersion (mean)  :    4.4513705754520139
 Standard Deviation          :    4.2426406871192848
 Minimum                     :    5.0000000000000000
 Maximum                     :    15.000000000000000
```

Write for sample 1:
```
x = 10.0; T = +/-10.9 for P = 95%; s = +/-4.243
```

Result in words:
*IIn the given sample distribution (sample 2, see below) 95% of all values are expected between -0.1 and 20.9 (range of dispersion of the values) but 68% between 5.8 and 14.2 (standard deviation). The mean "x" itself can spread between 5.5 and 14.5 (range of dispersion of the mean) with a probability of 95%.*

## Requirements ##

To **run** *sampleSTAT* you do not have to take care of any library dependencies. Binaries for Windows, GNU/Linux and macOS can be found in the [Release section](https://github.com/haniibrahim/sampleSTAT/releases).

To **build** this app you just need a Fortran 90 compiler with the Fortran 2003 routines `command_argument_count()` and `get_command_argument()` or the often available but non-standard `iargc()` and `getarg()` routines.

Compatible compilers are gfortran, g95, ifort and many more. You can compile and run sampleSTAT on almost all platforms (GNU/Linux, Microsoft Windows, Mac OS X, BSD, etc.)

Furthermore to have "make" is useful but not mandatory.

On **GNU/Linux** you need "gfortran" and "make". On **macOS** you should use *Homebrew* or *MacPorts* to install "gfortran" and "make". To build sampleSTAT on **Windows** you should have *mingw32*, *mingw64* or *Cygwin* installed.

## Build and Install ##

### On UNIX(-like) systems ###

To compile and install sampleSTAT on UNIX(-like) systems with gfortran:

```
gfortran -O3 -s -static sysconst_mdl.F90 dbg_mdl.F90 sng_mdl.F90 sngall_mdl.F90 samplestatistics_mdl.f90 readdata_mdl.f90 sampleSTAT.f90 -o sampleSTAT
sudo cp ./sampleSTAT /usr/local/bin/
sudo cp ./sampleSTAT.1 /usr/local/share/man/man1/
```

or easier via make:

```
make -f makefile.linux.mak
sudo make -f makefile.linux.mak install
```

### On macOS ###

```
gfortran -O3 -s -static-libgfortran -static-libgcc sysconst_mdl.F90 dbg_mdl.F90 sng_mdl.F90 sngall_mdl.F90 samplestatistics_mdl.f90 readdata_mdl.f90 sampleSTAT.f90 /opt/local/lib/gcc9/libquadmath.a -o sampleSTAT
sudo cp ./sampleSTAT /usr/local/bin/
sudo cp ./sampleSTAT.1 /usr/local/share/man/man1/
```

or

```
make -f makefile.mac.mak
sudo make -f makefile.mac.mak install
```

**IMPORTANT NOTE**: On macOS you have to adjust the path `/opt/local/lib/gcc9/libquadmath.a` in your commandline or in the file `gf90.mac.tpl` to your current environmemt. You can omit this path and the two `-static-...` options if you want. You just loose your independency to a gfortran installation.

### On Microsoft Windows ###

```
gfortran -O3 -s -static sysconst_mdl.F90 dbg_mdl.F90 sng_mdl.F90 sngall_mdl.F90 samplestatistics_mdl.f90 sampleSTAT.f90 readdata_mdl.f90 -o sampleSTAT.exe
```

or

```
make -f makefile.win.mak
```

For a quick and dirty installation, copy `sampleSTAT.exe` into your Windows folder or much better in another folder which is in your path.

With the linker option `-static` sampleSTAT should run on other machines without dependencies to gfortran's and gcc's libraries.

## Download binaries ##
Some precompiled binaries for some platforms are located in the [Release section](https://github.com/haniibrahim/sampleSTAT/releases).
