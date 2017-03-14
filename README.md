# STOMP-W
Fortran 90 source code, example problems, and output conversion scripts for the STOMP-W simulator.

STOMP-W is a numerical simulator written in Fortran 90 for solving problems involving fluid flow and solute/species transport in variably saturated geologic media. The zipped file contains five directories: 1) src (the source code), 2) scripts (output conversion scripts written in perl, 3) prb-w-1 (example problem 1), 4) prb-w-2 (example problem 2), and 5) prb-w-3 (example problem 3). After downloading and unzipping the file, the user must create an executable from the files stored in the src directory.  Generic instructions for compiling the source code is shown below as a UNIX/Linux command line, with f90 representing the user's Fortran 90 compiler.

## Compilation
* f90 -c -O2 allo.f
* f90 -c -O2 (listing of all other files ending in .f)
* f90 -O2 -o stomp-w.x

The above commands will create a file named stomp-w.x, which is the executable. After creating this executable, the user should test the simulator against the example problems. The example problems can be executed by migrating to the problem directory, copying the executable into the directory, and then issuing the command in the problem directory. Please note that the connect, output, plot, and surface files will be overwritten; therefore, it's recommended that the user make copies of these directories before executing the code.

## Output Scripts
There are three scripts provided to convert STOMP-W output into other forms: 1) outputTo.pl, 2) plotTo.pl, and 3) surfaceTo.pl. The output.pl script is used to convert output files. The plotTo.pl script is used to convert plot files. The surfaceTo.pl is used to convert surface files. Help on using the scripts can be found by issuing the script command in the script directory (e.g., outputTo.pl --help). Executing these scripts requires perl to be installed on the user's computer.

## Information
More information about the STOMP simulator and STOMP-W, including the User's Guide can be found online at http://stomp.pnnl.gov
