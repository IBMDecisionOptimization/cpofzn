# CpoFzn

CpoFzn is a CP Optimizer solver interface for [MiniZinc](http://www.minizinc.org). 
Through CpoFzn, models written in MiniZinc can be solved by 
[IBM ILOG CPLEX CP Optimizer](https://www.ibm.com/analytics/cplex-cp-optimizer).

## Getting Started

To get MiniZinc running with CpoFzn, follow these steps.

### Prerequisites

Unless you want to use a development version of CpoFzn, we suggest that you use one of versioned tarballs,
which can be found [here](https://www.github.com/IBMDecisionOptimization/cpofzn/releases).

You will need to have CPLEX Optimization Studio 20.1 installed.  If you don't have it installed already:

* If you are an academic, you can access CPLEX Optimization Studio for free [here](https://www.ibm.com/academic/technology/data-science),
* If you are a commercial user, you can find purchase options [here](https://www.ibm.com/products/ilog-cplex-optimization-studio)

Download and install MiniZinc from [here](http://www.minizinc.org) (release 2.4.2 or later).

You will also need some additional tools.

  * A C++ compiler of your choice,
  * CMake version 3.5 or later,
  * Flex version 2.6.4 or later,
  * Bison version 3.0.4 or later.

On UNIX variants most of these can be installed using a package tool like `dnf`, `yum`, `apt-get` or `brew`.

On Windows, you will most probably need to install a compiler such as Visual Studio.
Binaries for Flex and Bison can be found [here](https://sourceforge.net/projects/winflexbison).
Binaries for CMake can be found [here](http://cmake.org/download).

### Building the `cpofzn` (or `cpofzn.exe`) executable

We use CMake as a cross-platform build-generator tool. CMake does not build
the project, but generates the files needed by your build tool (GNU make,
Ninja, Visual Studio, Eclipse, etc). Please refer to CMake documentation for
details [here](https://cmake.org/runningcmake).

In most cases CMake detects the right build tool for your platform
automatically (assuming that you have some build tool and C++ compiler
installed). So the following steps should be enough:

  * Open shell in the root folder of this project.
  * Type "cmake ." or "cmake -DCPLEX_STUDIO_DIR=<path_to_your_installation> ."
  * Type "cmake --build ."

Alternatively you may use some cmake GUI frontend, e.g. cmake-gui.
CMake needs to know the installation directory of IBM CPLEX Optimization Studio.
If it is not detected automatically, you can specify using -DCPLEX_STUDIO_DIR
as mentioned above.

If you are on a Windows system, you may find the following BAT script useful.
```
   SET        CMAKE_EXE=<Path of the cmake.exe file>
   SET  FLEX_EXECUTABLE=<Path of the win_flex.exe file>
   SET BISON_EXECUTABLE=<Path of the win_bison.exe file>
   SET CPLEX_STUDIO_DIR=<Path of the CPLEX installation directory>
   REM Build code to compile
   "%CMAKE_EXE%" -D "FLEX_EXECUTABLE=%FLEX_EXECUTABLE%" ^
                 -D "BISON_EXECUTABLE=%BISON_EXECUTABLE%" ^
                 -D "CPLEX_STUDIO_DIR=%CPLEX_STUDIO_DIR%" ^
                 -D CMAKE_GENERATOR_PLATFORM=x64 .
   REM Compile 
   "%CMAKE_EXE%" --build . --clean-first --config Release
```
The resulting cpofzn.exe file can be found in the Release sub-directory.
You can safely ignore any warnings issued by your compiler.  They are harmless.


### Solver registration

Before solving a MiniZinc model using CP Optimizer, it is necessary to first
register CpoFzn in your MiniZinc installation. To do that:

1. During the build of the `cpofzn` executable a file named `com.ibm.cpo.msc`
   was generated.  This file contains information about locations of
   files needed by Minizinc.
2. If you open `com.ibm.cpo.msc` with an editor, you will see the fields
   `executable` and `mznlib`.  Should you move the referenced executable
   or the mznlib directory somewhere else, then you should change the relevant
   field(s) in the `com.ibm.cpo.msc` file accordingly.  Note that the paths
   should always be absolute.
3. Copy the file `com.ibm.cpo.msc` into one of the directories searched by MiniZinc for solver
configuration files (see the MiniZinc [documentation](https://www.minizinc.org/doc-2.5.0/en/fzn-spec.html#solver-configuration-files)):
    * `$HOME/.minizinc/solvers` on Linux and Mac OS
    * the `share/minizinc/solvers/` sub-directory of your MiniZinc installation,
    * in any directory listed in the `MZN_SOLVER_PATH` environment variable.

## Usage

Once registered in MiniZinc, `CPOptimizer` appears as one of the solvers
available in MiniZinc. In MiniZinc IDE, `CPOptimizer` can be chosen from the
list of solvers. Additionally, from the command line you can use:
```sh
minizinc --solver CPOptimizer
```

## Known issues

### Cycles in `defines_var`

Sometimes MiniZinc generates circular `defines_var` annotations during
flattening. Such circular definitions are refused by `cpofzn` with errors like
this one:
```
Error: /tmp/mznfileOB3mKO.fzn:23: There is cycle in the model: expression 'X_INTRODUCED_10_' is using itself as a subexpression.
/tmp/mznfileOB3mKO.fzn:988:    .. The cycle contains mutable expression 'X_INTRODUCED_1046_'.
/tmp/mznfileOB3mKO.fzn:43:    .. The cycle contains mutable expression 'X_INTRODUCED_34_'.
/tmp/mznfileOB3mKO.fzn:908:    .. The cycle contains mutable expression 'X_INTRODUCED_913_'.
```
The problem is not in `cpofzn` but in MiniZinc itself.

## Note about scheduling models

CP Optimizer uses _optional interval variables_ to model tasks in scheduling
however MiniZinc uses integer variables (e.g. to represent start times). For
this reason, when a MiniZinc variable is used in a scheduling constraint (in
particular `cumulative`) then `cpofzn` creates an interval variable that is
synchronized with the original integer variable. Therefore scheduling models
translated from MiniZinc are sub-optimal and it is recommended to use native CP
Optimizer model instead. Also optional types (`var opt int`) are transformed
into non-optionals at MiniZinc level. This is a second reason why the generated
model is suboptimal.

## Contact

You can post your questions on the IBM Optimization forum
[here](http://ibm.biz/DOForums) under the "Discussion" tab.

## Contributors

Some of this code is based on the FlatZinc parser skeleton
by Nick Nethercote and Julien Fischer delivered
with [G12 MiniZinc Distribution](https://www.minizinc.org/g12distrib.html) version 1.6.

## License

Licensed under the Apache License, version 2.0.
