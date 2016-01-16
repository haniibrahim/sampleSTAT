# sampleSTAT - Statistics for Sampling Distributions

**sampleSTAT** is a command-line application to determine sampling distributions. It goes a lot further than arithmetic mean and standard deviation. It uses floating point variables according [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point) which make results portable (see Chap. Accurracy). sampleSTAT is written in Fortran 90.

sampleSTAT is based on the German book *R.Kaiser, G. Gottschalk; "Elementare Tests zur Beurteilung von Meßdaten", BI Hochschultaschenbücher, Bd. 774, Mannheim 1972.*

## Purpose
Performs statistical tests for sampling distributions:
- Arithmetic Mean
- Standard Deviation
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

### Number of Values (N)###

The amount of single values (N) is a important degree to evaluate the security of the result. N specify the measuring expenditure but N itself has no significance to the the result. In both examples above N was the same.

### Standard Deviation (s) ###

The standard deviation is the most imformative quantity to specify the statistic quality of a measuring procedure but *s* itself does not say anything about the quality of the result but in combination with *n* you can calculate the range of dispersion.

### Range of Dispersion of Values (T) ###
The range of dispersion of the values is calculated as `T = s * t`. (t = student factor, dependent on the statistic security P% (95%, 99%, 99.9%) and the degree of freedom `f = n - 1`. *f* and *t* are specified in the t-tables.

*T* indicates that P% of all single values - with x as its mean - are expected around `x + T` and `x - T`. *T* is the parameter which specify the quality of the raw values.

### Range of Dispersion of Mean ###

This parameter tells how secure the mean is. It indicates the stray area of the mean and not of the raw values as *T* does.

## Accuracy

It uses [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point) *(IEEE Standard for Binary Floating-Point Arithmetic for microprocessor systems (ANSI/IEEE Std 754-1985))* real values which give the same results independently on which architecture (32 or 64-bit) the sources were compiled and run.

## Usage

Type `sampleSTAT --help` for help:

```
sampleSTAT performs tests for statistical samples:
   Aritmetic Mean, Range of Dispersion of values and mean based on t-factor,
   Standard Deviation, Minimum, Maximum.

Usage: sampeSTAT [-hv] -s X [<inputfile] [>outputfile]
  -h    --help /?   Print this help screen
  -v    --version   Print version information
  -s X  --sens=X    Set confidence level:
                    X=0 conf. level: 95%
                    X=1 conf. level: 99%
                    X=2 conf. level: 99.9%

Examples:
  sampleSTAT -s 0 <mydata.dat
  sampleSTAT --sens=1 <mydata.dat >results.txt

Input data:
  Data has to be committed in a one column form, like:
     22.43
     22.45
     22.50

```
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
$ sampleSTAT -s 0 <sample1.dat 

     sampleSTAT - Statistics for Sampling Distributions
     ==================================================
     Number of Values            :            6
     Arithmetic Mean             :    10.000000000000000     
     Confidence Level            :  95%
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
*In the given sample distribution (sample 1) 95% of all values will be between 0 and 20.9 (range of dispersion of the values) but 63% between 5.8 and 14.2 (standard deviation => 63%). The mean "x" itself can spread between 14.08 and 5.92 (range of dispersion of the mean). *



Note: 6 values in this example are too less to give a proper view for 95% of all. 

## Requirements

To compile this app you just need a Fortran 90 compiler with the Fortran 2003 routines `command_argument_count()` and `get_command_argument()` or the often available but non-standard `iargc()` and `getarg()` routines.

Compatible compilers are gfortran, g95, ifort and many more. You can compile and run sampleSTAT on almost all platforms (GNU/Linux, Microsoft Windows, Mac OS X, BSD, etc.)

## Build and Install

To compile and install sampleSTAT on UNIX(-like) systems with gfortran:

```
gfortran -static-libgfortran dbg_mdl.F90 sng_mdl.F90 sngall_mdl.F90 sysconst_mdl.f90 samplestatistics_mdl.f90 sampleSTAT.f90 -o sampleSTAT
sudo cp ./sampleSTAT /usr/local/bin/
```

or easier via make:

```
make
sudo make install
```

On Microsoft Windows:

```
gfortran -static-libgfortran dbg_mdl.F90 sng_mdl.F90 sngall_mdl.F90 sysconst_mdl.f90 samplestatistics_mdl.f90 sampleSTAT.f90 -o sampleSTAT.exe
```
For a quick and dirty installation, copy `sampleSTAT.exe` into your WIndows folder or much better in another folder which is in your path.