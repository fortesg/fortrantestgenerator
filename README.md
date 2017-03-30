# FortranTestGenerator

`fortrantestgenerator` (FTG) is a tool for automatically generating unit tests for subroutines of existing Fortran applications based on an approach called Capture & Replay.

One of the main effort for creating unit tests is the set-up of consistent input data. When working with legacy code, we can make use of the existing infrastructure and extract test data from the running application. 
FTG generates code for serializing and storing a subroutines input data and inserts this code temporarily into the subroutine (capture code).
In addition, FTG generates a basic test driver which loads this data and runs the subroutine (replay code). 
Meaningful checks and test data modification needs to be added by the developer.

The principles of FTG are described in the following paper:

> C. Hovy and J. Kunkel, "Towards Automatic and Flexible Unit Test Generation for Legacy HPC Code," *2016 Fourth International Workshop on Software Engineering for High Performance Computing in Computational Science and Engineering (SE-HPCCSE)*, Salt Lake City, UT, 2016, pp. 1-8.
> http://dx.doi.org/10.1109/SE-HPCCSE.2016.005

## In general it works as follows

1. You identify an existing subroutine in your Fortran application and a certain execution of this subroutine that you want to run for test purposes in isolation, that is without the surrounding application.

2. You run FTG to insert the capture code into the subroutine. This code is responsible for storing all input variables to you hard drive when the capturing is active. Thanks to a built-in static source code analysis, only those variable are captured that are actually needed by the subroutine or by one of its directly or indirectly called routines.

3. Then you define the event on which the capturing should take place. By default, it's the first execution of the subroutine.

4. You compile and run your application with the capture code.

5. You run FTG to create the replay code, that means a basis test driver which loads the captured data and calls the subroutine by passing the captured data as input.

Variables that are considered to be input data:
* Arguments of intrisic types
* Components of derived type arguments that are actually used by the subroutine
* Module variables of the same module or imported by USE statements that are actually used by the subroutine

