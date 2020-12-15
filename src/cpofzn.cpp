// --------------------------------------------------------------------------
// Source file provided under Apache License, Version 2.0, January 2004,
// http://www.apache.org/licenses/
// (c) Copyright IBM Corp. 2019, 2020
// --------------------------------------------------------------------------

#include "fznreader.h"
#include <chrono>
#include <csignal>

using namespace cpoptimizer;

void usage() {
  std::cerr <<
    "CP Optimizer flatzinc interface. Build with with CP Optimizer version " << IloCP::GetVersion() << ".\n"
    "\n"
    "Usage: cpofzn [options] <file.fzn>\n"
    "\n"
    "Standard options:\n"
    " -a\n"
    "   Satisfaction problems: report all solutions.\n"
    "   Optimization problems: print solutions of increasing quality as they are found.\n"
    " -n <i>\n"
    "   Stop after reporting i solutions.\n"
    " -f\n"
    "   Ignore any search annotations (currently the default).\n"
    " -s\n"
    "   Print statistics during and after the search for solutions.\n"
    " -v\n"
    "   Print CP Optimizer log to the standard error stream.\n"
    " -p <i>\n"
    "   Run with i parallel threads (parameter Workers, default: 1).\n"
    "   CP Optimizer specific: value 0 means use all available cores.\n"
    " -r <i>\n"
    "   Use i as the random seed (parameter RandomSeed).\n"
    " -t <ms>\n"
    "   Wall time limit ms milliseconds (parameter TimeLimit).\n"
    "\n"
    "CP Optimizer specific options:\n"
    " -h\n"
    "   Display this help.\n"
    " -o <model.cpo>\n"
    "   Instead of solving export the model into specified file using cpo file format.\n"
    " -d <model.cpo>\n"
    "   Before solving the model dump it into the specified file using cpo file format.\n"
    " -<parameter name> <value>\n"
    "   Specify value of a CP Optimizer parameter. E.g. '-DefaultInferenceLevel Extended'.\n"
    " -c <config.msc> <executable> <builddir>\n"
    "   Generate MiniZinc solver configuration file for CP Optimizer (used during build)."
    << std::endl;
  exit(1);
}

inline IlcBool readInt(const char* src, IlcInt* result) {
  errno = 0; // Clear error flags before calling strtol
  char* endPtr = NULL;
  *result = strtol(src, &endPtr, 10);
  return errno == 0 && (!endPtr || !*endPtr);
}

inline IlcBool readFloat(const char* src, IlcFloat* result) {
  errno = 0; // Clear error flags before calling strtol
  char* endPtr = NULL;
  *result = strtod(src, &endPtr);
  return errno == 0 && (!endPtr || !*endPtr);
}

void printInitStats(CP cp) {
  std::cout
    << "%%%mzn-stat: initTime=" << cp.getInfo(IloCP::ExtractionTime) << '\n'
    << "%%%mzn-stat: variables=" << cp.getInfo(IloCP::NumberOfVariables) << '\n'
    << "%%%mzn-stat: propagators=" << cp.getInfo(IloCP::NumberOfConstraints) << '\n';
  if (cp.hasObjective())
    std::cout << "%%%mzn-stat: objectiveBound=" << cp.getObjBound() << '\n';
  std::cout
    << "%%%mzn-stat: peakMem=" << cp.getInfo(IloCP::MemoryUsage) / 1024 / 1024 << '\n'
    << "%%%mzn-stat-end" << std::endl;
}

void printSearchStats(IloCP cp, IlcBool hadSolution) {
  std::cout
    << "%%%mzn-stat: nodes=" << cp.getInfo(IloCP::NumberOfBranches) << '\n'
    << "%%%mzn-stat: solveTime=" << cp.getInfo(IloCP::TotalTime) << '\n';
  if (cp.hasObjective()) {
    if (hadSolution)
      std::cout << "%%%mzn-stat: objective=" << cp.getObjValue() << '\n';
    std::cout << "%%%mzn-stat: objectiveBound=" << cp.getObjBound() << '\n';
  }
  std::cout
    << "%%%mzn-stat: failures=" << cp.getInfo(IloCP::NumberOfFails) << '\n'
    << "%%%mzn-stat: peakMem=" << cp.getInfo(IloCP::MemoryUsage) / 1024 / 1024 << '\n'
    << "%%%mzn-stat-end" << std::endl;
}

// Command line arguments
// For numeric parameters value -1 means the parameter was not set
struct Parameters {
  IlcBool     _printAll = IlcFalse;
  IlcInt      _solutionLimit = -1;
  IlcBool     _ignoreSearch = IlcFalse;
  IlcBool     _printStats = IlcFalse;
  IlcBool     _printLog = IlcFalse;
  // Minizinc do not pass -p parameters if user specifies 1 threads.
  // So the default must be 1:
  IlcInt      _threads = 1;
  IlcInt      _randomSeed = -1;
  IlcFloat    _timeLimitMS = -1;
  const char* _outputFilename = NULL;
  const char* _inputFilename = NULL;
  const char* _dumpFilename = NULL;
  const char* _configFile = NULL;
  const char* _executable = NULL;
  const char* _builddir = NULL;
};

static Parameters parseCommandLine(CP cp, int argc, const char** argv) {
  Parameters result;
  int i = 1;
  if (argc == 1)
    usage(); // There is no argument, avoid treating it as an error
  for (; i < argc; i++) {
    if (!strcmp(argv[i], "-a"))
      result._printAll = IlcTrue;
    else if (!strcmp(argv[i], "-f"))
      result._ignoreSearch = IlcTrue;
    else if (!strcmp(argv[i], "-s"))
      result._printStats = IlcTrue;
    else if (!strcmp(argv[i], "-v"))
      result._printLog = IlcTrue;
    else if (!strcmp(argv[i], "-h"))
      usage();
    else if (!strcmp(argv[i], "-n") || !strcmp(argv[i], "-p") || !strcmp(argv[i], "-r")) {
      IlcInt value;
      if (i+1 >= argc || !readInt(argv[i+1], &value) || value < 0) {
        std::cerr << "Parameter " << argv[i] << " requires non-negative integer argument." << std::endl;
        usage();
      }
      if (!strcmp(argv[i], "-n"))
        result._solutionLimit = value;
      else if (!strcmp(argv[i], "-p"))
        result._threads = value;
      else {
        assert(!strcmp(argv[i], "-r"));
        result._randomSeed = value;
      }
      i++;
    } else if (!strcmp(argv[i], "-t")) {
      IlcFloat value;
      if (i+1 >= argc || !readFloat(argv[i+1], &value) || value < 0) {
        std::cerr << "Parameter " << argv[i] << " requires non-negative floating-point argument." << std::endl;
        usage();
      }
      result._timeLimitMS = value;
      i++;
    } else if (!strcmp(argv[i], "-o")) {
      if (i+1 >= argc) {
        std::cerr << "Parameter " << argv[i] << " requires an argument." << std::endl;
        usage();
      }
      result._outputFilename = argv[i+1];
      i++;
    } else if (!strcmp(argv[i], "-d")) {
      if (i+1 >= argc) {
        std::cerr << "Parameter " << argv[i] << " requires an argument." << std::endl;
        usage();
      }
      result._dumpFilename = argv[i+1];
      i++;
    } else if (!strcmp(argv[i], "-c")) {
      if (i+3 >= argc) {
        std::cerr << "Parameter " << argv[i] << " requires 3 arguments." << std::endl;
        usage();
      }
      result._configFile = argv[i+1];
      result._executable = argv[i+2];
      result._builddir = argv[i+3];
      i += 3;
    } else if (argv[i][0] == '-') {
      // Attempt to parse -<parameter name> <value>
      if (i+1 >= argc) {
        // <value> is missing. Assume that the last argument is actually a filename (even though it starts with -).
        break;
      }
      try {
        cp.setParameter(argv[i]+1, argv[i+1]);
      } catch (IloException& ex) {
        std::cerr << "Error: " << ex << std::endl;
        exit(1);
      }
      i++;
    } else
      break;

  }
  if (!result._configFile) {
    // At this point, there should be only one final argument: the name of the flatzinc file.
    if (i == argc) {
      std::cerr << "Missing input flatfzinc file name.\n";
      usage();
    }
    if (i < argc - 1) {
      std::cerr << "Unknown argument: " << argv[i] << '\n';
      usage();
    }
    result._inputFilename = argv[i];
  } else {
    // Generating config file. There shouldn't be any fzn file.
    if (i < argc) {
      std::cerr << "Too many arguments when generating config file.\n";
      usage();
    }
  }
  return result;
}

// Remember CP in global variable for signal handler below
static CP* mainCP = nullptr;

// Signal handler e.g. for ctrl-c. Stop the search:
void whenSignal(int signal) {
  if (mainCP) {
    mainCP->abortSearch();
    // If we get the signal again then call exit
    mainCP = nullptr;
  } else
    exit(signal);
}

static OutputSpecification readFzn(CP& cp, const char* filename) {
  std::ifstream stream(filename);
  if (stream.fail()) {
    std::cerr << "Can't open file '" <<  filename << "'.\n";
    exit(1);
  }
  FznReader reader(cp, stream, filename);
  if (!reader.read())
    exit(1);
  return reader.getOutputSpecification();
}

static void generateConfigFile(IloCP cp, const char* cfgfilename, const char* executable, const char* builddir) {
  std::ofstream stream(cfgfilename);
  if (stream.fail()) {
    std::cerr << "Can't open file '" << cfgfilename << "' for writing.\n";
    exit(1);
  }
  stream <<
    "{\n"
    "    \"executable\": \"" << executable << "\",\n"
    "    \"id\": \"com.ibm.cpo\", \n"
    "    \"isGUIApplication\": false, \n"
    "    \"mznlib\": \"" << builddir << "/mznlib" << "\",\n"
    "    \"mznlibVersion\": 1, \n"
    "    \"name\": \"CPOptimizer\", \n"
    "    \"needsMznExecutable\": false, \n"
    "    \"needsPathsFile\": false, \n"
    "    \"needsSolns2Out\": true, \n"
    "    \"needsStdlibDir\": false, \n"
    "    \"stdFlags\": [ \n"
    "        \"-a\", \n"
    "        \"-n\", \n"
    "        \"-p\", \n"
    "        \"-s\", \n"
    "        \"-v\", \n"
    "        \"-r\", \n"
    "        \"-f\", \n"
    "        \"-t\" \n"
    "    ], \n"
    "    \"supportsFzn\": true, \n"
    "    \"supportsMzn\": false, \n"
    "    \"supportsNL\": false, \n"
    "    \"version\": \"" << cp.getVersion() << "\"\n"
    "}";
}

int main(int argc, const char** argv) {

  IloEnv env;
  CP cp(env);

  Parameters params = parseCommandLine(cp, argc, argv);

  if (params._configFile) {
    generateConfigFile(cp, params._configFile, params._executable, params._builddir);
    std::cout << "Generated config file '" << params._configFile << "'." << std::endl;
    return 0;
  }

  // Redirect standard log:
  cp.setOut(std::cerr);
  // By default log is off:
  if (!params._printLog)
    cp.setParameter(IloCP::LogVerbosity, IloCP::Quiet);

  IlcInt result = 0;

  try {

    auto startTime = std::chrono::system_clock::now();
    OutputSpecification outputSpecification = readFzn(cp, params._inputFilename);
    if (params._printLog) {
      auto endTime = std::chrono::system_clock::now();
      std::chrono::duration<double> elapsed_seconds = endTime - startTime;
      std::ios_base::fmtflags originalFormatting(std::cerr.flags());
      std::cerr << " ! FlatZinc parse time: " << std::fixed << std::setprecision(2) << elapsed_seconds.count() << "s" << std::endl;
      std::cerr.flags(originalFormatting);
    }

    // Setup CPO parameters
    if (params._solutionLimit != -1)
      cp.setParameter(IloCP::SolutionLimit, params._solutionLimit);
    if (params._threads > 0)
      cp.setParameter(IloCP::Workers, params._threads);
    else
      cp.setParameter(IloCP::Workers, IloCP::Auto);
    if (params._randomSeed != -1)
      cp.setParameter(IloCP::RandomSeed, params._randomSeed);
    if (params._timeLimitMS != -1)
      cp.setParameter(IloCP::TimeLimit, params._timeLimitMS / 1000);

    if (params._dumpFilename)
      cp.dumpModel(params._dumpFilename);

    // Do the main task:
    if (params._outputFilename) {
      // Dump the model in CPO file format:
      cp.exportModel(params._outputFilename);
    } else {
      // Solve it. But first install our own signal handler. This way we stop
      // the search nicely and we can print e.g. the final statistics.
      std::signal(SIGINT, whenSignal);
      mainCP = &cp;

      cp.startNewSearch();
      if (params._printStats)
        printInitStats(cp);

      IlcBool hadSolution = IlcFalse;

      while (cp.next()) {
        hadSolution = IlcTrue;
        if (params._printStats)
          printSearchStats(cp, hadSolution);
        // Print the solution if all of them should be printed:
        if (params._printAll)
          outputSpecification.print(cp);
        else if (!cp.hasObjective())
          break; // We are looking for just one solution of a decision problem so we can stop
      }
      cp.endSearch();
      mainCP = nullptr;
      if (params._printStats)
        printSearchStats(cp, hadSolution);
      // Print final solution if we don't print all solutions:
      if (!params._printAll && hadSolution)
        outputSpecification.print(cp);
      // Print final line (whether we have proof):
      if (cp.getInfo(IloCP::SearchStatus) == IloCP::SearchCompleted) {
        if (hadSolution) {
          if (params._printAll || cp.hasObjective())
            std::cout << "==========" << std::endl;
        } else
          std::cout << "=====UNSATISFIABLE=====" << std::endl;
      }
      else if (!hadSolution && cp.hasObjective())
        std::cout << "=====UNKNOWN=====" << std::endl;
    }
  }
  catch (IloException &ex) {
    std::cerr << "Error: " << ex << std::endl;
    result = 1;
  }

  env.end();

  return result;
}
