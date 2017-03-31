# FortranTestGenerator

`FortranTestGenerator` (FTG) is a tool for automatically generating unit tests for subroutines of existing Fortran applications based on an approach called Capture & Replay.

One of the main effort for creating unit tests is the set-up of consistent input data. When working with legacy code, we can make use of the existing infrastructure and extract test data from the running application. 
FTG generates code for serializing and storing a subroutines input data and inserts this code temporarily into the subroutine (capture code).
In addition, FTG generates a basic test driver which loads this data and runs the subroutine (replay code). 
Meaningful checks and test data modification needs to be added by the developer.
All the code generated by FTG is based on customizable templates. So you are able to adapt it to your software environment.

FTG is written in Python and the principles of FTG are described in the following paper:

> C. Hovy and J. Kunkel, "Towards Automatic and Flexible Unit Test Generation for Legacy HPC Code," *2016 Fourth International Workshop on Software Engineering for High Performance Computing in Computational Science and Engineering (SE-HPCCSE)*, Salt Lake City, UT, 2016, pp. 1-8.
> http://dx.doi.org/10.1109/SE-HPCCSE.2016.005

So far, the documentation is very poor. If your interested in using `FortranTestGenerator`, please feel free to contact me:   
Christian Hovy <<hovy@informatik.uni-hamburg.de>>

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

For the source code analysis, FTG uses the tool [FortranCallgraph](https://github.com/chovy1/fortrancallgraph), which needs assembler files generated by [gfortran](https://gcc.gnu.org/fortran) for the analysis.

## Prerequisites 

To run FTG, you will need the following software packages:

* `Python 2.7` (Python 3+ is currently not supported, but all uncompatible stuff will be removed in the future)
* The `Cheetah Template Engine`: https://github.com/cheetahtemplate/cheetah (Unfortunately, development and support here has stopped during the development of FTG, but there is now a fork on: https://github.com/CheetahTemplate3/cheetah3, which has not yet been tested with FTG though.)
* This modified version of the `SerialBox` library: https://github.com/chovy1/serialbox
* `FortranCallgraph`: https://github.com/chovy1/fortrancallgraph

## Quick Start Guide

#### 1. Get and install `SerialBox`
... from here: https://github.com/chovy1/serialbox and learn how to built your application with it. 

#### 2. Get and install the `Cheetah Template Engine`
...from here: https://github.com/cheetahtemplate/cheetah or here: https://github.com/CheetahTemplate3/cheetah3 or just look if your OS provides a package (e.g. Ubuntu does).

#### 3. Get `FortranCallgraph` 
...from here: https://github.com/chovy1/fortrancallgraph

#### 4. Configure and try `FortranCallgraph`
...according to its [documentation](https://github.com/chovy1/fortrancallgraph/blob/master/README.md).

#### 5. Clone this repo

```
$> git clone https://github.com/chovy1/fortrantestgenerator.git
$> cd fortrantestgenerator
```

#### 6. Fill out the configuration file [config_fortrantestgenerator.py](config_fortrantestgenerator.py)

`FTG_DIR` : The location of FortranTestGenerator (usually `os.path.dirname(os.path.realpath(__file__))`)

`FCG_DIR` : The location of FortranCallgraph (usually `FTG_DIR + '../fortrancallgraph'')

`TEMPLATE_DIR` : The location of the templates to be used for code generation (`FTG_DIR + '/templates/standalone_nompi'` shall be a good start, or `FTG_DIR + '/templates/standalone'` if your application uses MPI)

`TEST_SOURCE_DIR` : The folder where the generated test driver shall be put in.

`TEST_DATA_BASE_DIR` : The folder where the captured data shall be put in.

**Please note:** If you don't want to have the configuration spread over the two files of FortranCallgraph and FortranTestGenerator, you can put all the variables from config_fortrancallgraph.py into config_fortrantestgenerator.py instead of importing them.

#### 7. Create assembler files

Compile your Fortran application with [gfortran](https://gcc.gnu.org/fortran) and the options `-S -g -O0` to generate assembler files.

#### 8. Create capture code

Let's assume your subtroutine under test is the subroutine `my_subroutine` from the module `my_module`. Just run:

```
$> ./FortranTestgenerator.py -c my_module my_subroutine
```
#### 9. Define capture event

Have a look at the generated code in the module file where the subroutine under test (`my_subroutine`) is located.
When using one of the provided templates, there are now the two functions: `ftg_my_subroutine_capture_input_active` and `ftg_my_subroutine_capture_output_active`. Those functions define when the time is come to capture the subroutines' input and output. 

By default, both functions just compare the variable `ftg_velocity_tendencies_round`, in which the subroutine executions are counted, with the variable `ftg_velocity_tendencies_capture_round`. By default, `ftg_velocity_tendencies_capture_round` is set to `1`, which means that the capturing takes place in the first execution of `my_subroutine`.

If you want the capturing to happen for example in the 42nd execution of `my_subroutine`, just set `ftg_velocity_tendencies_capture_round` to `42`, but you can also change the functions to what ever you like. If you want to make the time for capturing dependent on the status of another variable, you can also add arguments to those functions. Of course, then you need to add the arguments also at the places where the functions are called.

#### 10. Create folders for the captured data

You will need the following directories for capturing data from `my_subroutine`:
* `TEST_DATA_BASE_DIR/ftg_my_subroutine_test/input`
* `TEST_DATA_BASE_DIR/ftg_my_subroutine_test/output`
* `TEST_DATA_BASE_DIR/ftg_my_subroutine_test/output_test`

`TEST_DATA_BASE_DIR` stands for the path set in the configuration file. `my_subroutine` has to be replaced by the actual subroutine name.

It is a little bit annoying that you have to create these folders manually, but currently there is no other option.

#### 11. Compile and run your application with the capture code

This will only work if you have added the includes and libraries of SerialBox to your build configuration, see step 1.

When the capturing is taking place, there will be messages printed to `stdout` beginning with `FTG...`.

When each MPI process has printed `FTG FINALIZE OUTPUT DATA my_subroutine`, capturing has finished and you can kill your application.

#### 12. Create replay code
Run:
```
$> ./FortranTestgenerator.py -r my_module my_subroutine
```
#### 13. Compile and run the generated test driver (replay code)

You have to run the test with the same numbers of MPI processes as you have done for capturing.

#### 14. Compare the original output with the output from the test

The original output is located in `TEST_DATA_BASE_DIR/ftg_my_subroutine_test/output` and the test output was put into `TEST_DATA_BASE_DIR/ftg_my_subroutine_test/output_test`.

To compare the data, you can use `serialbox-compare`: https://github.com/chovy1/serialbox-compare.

Do the following:
```
$> cd TEST_DATA_BASE_DIR/ftg_my_subroutine_test
$> sbcompare output/ftg_my_subroutine_output_0.json output_test/ftg_my_subroutine_output_0.json
```
This compares the output for the first MPI process. Replace `_0` by `_1`, `_2`, etc. for comparing the output of the other processes. 

If deviations are shown, it's up to you to figure out what went wrong, for example if one variable was missed by the source code analysis or if there is some kind of non-determinism in your code.

#### 15. Make a real test out of the generated test driver

For example add some checks, modify the loaded input data and run the subroutine under test again etc.

You should also remove the dependencies to the capture code, so that you can remove that stuff from the subroutine and its module.

If you want to load the original output data for your checks, just have a look how this is done for the input data.

## Please Note

* `FortranTestGenerator.py -c` not only generates the capture code in the module with the subroutine under test, but also a PUBLIC statements in every module that contains a module variable that is needed by the test and not yet public (export code).
This only works for module variables that are private because the whole module is private and they are not explicitly set to public. If a variable is private because it has the private keyword in its declaration, this procedure won't work and you have to manually make them public. The compiler will tell you if there is such a problem. Similar problems can occure elsewhere.

* For each module that is modified by `FortranTestGenerator.py -c` a copy of the original version is created with the file ending `.ftg-backup`. You can restore these backups by running
  ```
  $> ./FortranTestGenerator.py -b
  ```
* You can combine the options `-b`, `-c` and `-r` in any combination. When running `FortranTestGenerator.py` with `-b` option, restoring the backups will always be the first action, and when running with `-r`, generating the replay code will come at last.

* `-b` will restore all backups, so also the generated PUBLIC statements will be removed, but usually, you will need them for your test. So, if you want any generated code to stay, just remove the corresponding .ftg-backup file. It then might make sense to add some preprocessor directives around the generated code (e.g. something like `#ifdef __FTG_TESTS_ENABLED__ ... #endif`). If you want to have such directives always be there, just add them to the template you are using.

* As long as there is a backup file, any analysis is done on this instead of the original file.

* As mentioned before, the static source code analysis is done by [FortranCallgraph](https://github.com/chovy1/fortrancallgraph) which combines an analysis of assembler files with an analysis of the original source code. Actually, it first creates a call graph with the subroutine under test as root by parsing the assembler files and then it traverses this call graph while analysing the original (unpreprocessed) source files. This procedure can lead to problems if your code contains too much preprocessor acrobatics. 

  And there also are other cases where the assembler code differs from the orginal source code. Example:
  ```fortran
  LOGICAL, PARAMETER check = .TRUE.
  IF (check) THEN
    CALL a()
  ELSE
    CALL b()
  END IF
  ```
  Even when compiled with `-O0`, the `ELSE` block in this example won't be in the assembler/binary code. But usually this is not a problem, there will just be a warning during the analysis that the subroutine `b` is not found in the call graph.
  
* When you change your code, you will have to compile again with `-S -g -O0` to generate new assembler files. For example, when you have generated capture and export code and removed some backup files to make the code permanent, you have to compile again.

* If any problem occurs, please feel free to contact me:   
Christian Hovy <<hovy@informatik.uni-hamburg.de>>

## License

[GNU General Public License v3.0](LICENSE)
