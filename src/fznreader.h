// --------------------------------------------------------------------------
// Source file provided under Apache License, Version 2.0, January 2004,
// http://www.apache.org/licenses/
// (c) Copyright IBM Corp. 2019, 2020
// --------------------------------------------------------------------------

#ifndef FznReader_H
#define FznReader_H

#include <unordered_map>
#include <unordered_set>
#include <memory>

#define IL_STD
#define ILOUSEMT
#define _REENTRANT
#define ILM_REENTRANT
#include <ilcp/modeling.h>

ILCGCCHIDINGON

typedef std::vector<bool>     BoolVector;
typedef std::vector<IlcInt>   IntVector;
typedef std::vector<IlcFloat> FloatVector;

typedef std::vector<cpoptimizer::BoolVar>   BoolVarVector;
typedef std::vector<cpoptimizer::BoolExpr>  BoolExprVector;
typedef std::vector<cpoptimizer::IntVar>    IntVarVector;
typedef std::vector<cpoptimizer::IntExpr>   IntExprVector;
typedef std::vector<cpoptimizer::FloatExpr> FloatExprVector;

class Domain {
 public:
  enum Type {
    Boolean,
    Integer,
    Float
  };
 private:
  Type      _type;
  IlcFloat  _min;
  IlcFloat  _max;
  IntVector _values;

 public:
  Domain(Type type):
    _type(type)
  {
    switch (_type) {
      case Boolean:
        _min = 0;
        _max = 1;
        break;
      case Integer:
        _min = IlcIntMin;
        _max = IlcIntMax;
        break;
      case Float:
        _min = -IlcInfinity;
        _max = IlcInfinity;
        break;
      default:
        assert(false);
    }
  }
  Domain(IlcInt min, IlcInt max):
    _type(Integer),
    _min(min),
    _max(max)
  {}
  Domain(IlcFloat min, IlcFloat max):
    _type(Float),
    _min(min),
    _max(max)
  {}
  bool isBoolean() const {
    return _type == Boolean;
  }
  bool isInteger() const {
    return _type == Integer;
  }
  bool isFloat() const {
    return _type == Float;
  }
  void add(IlcInt value) {
    assert(_type == Integer);
    _values.push_back(value);
    if (value > _max)
      _max = value;
    if (value < _min)
      _min = value;
  }
  IlcInt getIntMin() const {
    assert(_type == Integer || _type == Boolean);
    return (IlcInt)_min;
  }
  IlcInt getIntMax() const {
    assert(_type == Integer || _type == Boolean);
    return (IlcInt)_max;
  }
  IlcFloat getFloatMin() const {
    assert(_type == Float);
    return _min;
  }
  IlcFloat getFloatMax() const {
    assert(_type == Float);
    return _max;
  }
  bool isEnumerated() const {
    assert(_type == Integer);
    return !_values.empty();
  }
  const IntVector& getEnumeratedValues() const {
    assert(isEnumerated());
    return _values;
  }
};

struct IntRange {
  IlcInt _min;
  IlcInt _max;
};

typedef std::vector<IntRange> IntRangeVector;
typedef std::unique_ptr<IntRangeVector> IntRangeVectorPtr;

// Location in the source code. We don't have multi-line terminal symbols. So
// in flex we compute location as line + two columns (first and last). In bison
// we compute location of non-terminals from locations of underlying terminals.
// So we can have multi-line locations then. However we don't use them anyway,
// in errors and warnings we use only starting line and starting column. So in
// fact _lastColumn is only used by flex because flex needs it in order to
// compute _firstColumn of the next terminal.
struct Location {
  IlcInt _line;
  IlcInt _firstColumn;
  IlcInt _lastColumn;
};

// For flex and bison:
#define YYLTYPE Location

struct ConstraintAnnotations {
  cpoptimizer::OptFloatExpr _definedVar;
  Location _definesVarLoc;
  ConstraintAnnotations(cpoptimizer::FloatExpr expr, const Location& where):
    _definedVar(expr),
    _definesVarLoc(where)
  {}
  ConstraintAnnotations() {}
};

struct VarAnnotations {
  bool              _isDefinedVar;
  Location          _definedLocation;
  bool              _isOutputVar;
  IntRangeVectorPtr _outputRanges;

  VarAnnotations():
    _isDefinedVar(false),
    _isOutputVar(false),
    _outputRanges(nullptr)
  {}
};

// A class representing one output line as specified by flatzinc solver interface.
// The output line could be a single value, for example:
//   x = 1;
// Or an array, for example:
//   xs = array1d(1..2, [1, 2]);
// For each output line we remember:
// * Name. Flatzinc output specification allows to print the same variable
//   multiple times under different names.
// * Expression to be printed (it could be an array).
// * Array of ranges to be printed in case of array.
class OutputLine {
 private:
  std::string       _name;   // Name printed. Should also the name of the object or one of its aliases.
  cpoptimizer::Expr _expr;   // Expression or an array of expressions
  IntRangeVectorPtr _ranges; // Ranges in case of an array

 public:
  OutputLine(const char* name, cpoptimizer::Expr expr, IntRangeVector* ranges = nullptr):
    _name(name),
    _expr(expr),
    _ranges(ranges)
  {}
  void print(cpoptimizer::CP cp);
};

typedef std::vector<OutputLine> OutputLineVector;

// Output specification is not part of FznReader because it has longer
// life span: fzn reader is deleted once the file is read, output specification
// must stay alive during the solve.
class OutputSpecification {
 private:
  OutputLineVector  _lines;
 public:
  void addLine(const char* name, cpoptimizer::Expr expr, IntRangeVector* ranges = nullptr) {
    _lines.emplace_back(name, expr, ranges);
  }
  void print(cpoptimizer::CP cp) {
    for (OutputLine& line: _lines)
      line.print(cp);
    std::cout << "----------" << std::endl;
  }
};

// The following union is for bison: it keeps a value for every terminal or
// non-terminal in the language. Note that names of the fields bellow reflects
// FlatZinc language, however their types are CP Optimizer types and they do
// not have to be equivalent. As the result there is for example _intVarArray
// but its type is cpoptimizer::IntExprArray.
union FznSemantics {
  // Various types of constants:
  char*                   _string;
  bool                    _bool;
  IlcInt                  _int;
  IlcFloat                _float;
  IntRange                _intRange;
  // CP Optimizer does not have BoolArray. So we use cpoptimizer::IntArray for it instead:
  cpoptimizer::IntArray   _boolArray;
  cpoptimizer::IntArray   _intArray;
  cpoptimizer::FloatArray _floatArray;
  // Expressions:
  cpoptimizer::BoolExpr   _boolExpr;
  cpoptimizer::IntExpr    _intExpr;
  cpoptimizer::FloatExpr  _floatExpr;
  // Named constants:
  cpoptimizer::BoolConstant  _boolConstant;
  cpoptimizer::IntConstant   _intConstant;
  cpoptimizer::FloatConstant _floatConstant;
  // Expression arrays:
  cpoptimizer::BoolExprArray  _boolVarArray;
  cpoptimizer::IntExprArray   _intVarArray;
  cpoptimizer::FloatExprArray _floatVarArray;
  // Parsed domain in definition of a variable:
  Domain*                 _domain;
  // Lists of constants or expressions for construction of arrays:
  IntVector*              _intList;
  FloatVector*            _floatList;
  BoolExprVector*         _boolVarList;
  IntExprVector*          _intVarList;
  FloatExprVector*        _floatVarList;
  // Annotations:
  ConstraintAnnotations*  _constraintAnnotations;
  VarAnnotations*         _variableAnnotations;
  IntRangeVector*         _outputRanges;

  // Following functions are necessary but not automatically generated:
  FznSemantics() {}
  FznSemantics& operator=(const FznSemantics& other) {
    memcpy((void*)this, (void*)&other, sizeof(*this));
    return *this;
  }
};

// For flex and bison:
#define YYSTYPE FznSemantics

// In case of cumulative constraint we are going to introduce interval
// variables that are associated with integer variables. To avoid creation of
// identical interval variables we keep existing variables in a map. The key
// into this map is the following class:
class IntervalVarKey {
 private:
  cpoptimizer::IntExpr _start;
  cpoptimizer::IntExpr _duration;
 public:
  IntervalVarKey(cpoptimizer::IntExpr start, cpoptimizer::IntExpr duration):
    _start(start), _duration(duration)
  {}
  const void* getStartImpl() const {
    return _start.getImpl();
  }
  const void* getDurationImpl() const {
    return _duration.getImpl();
  }
  bool operator==(const IntervalVarKey &other) const {
   return other._start.getImpl() == _start.getImpl()
     && other._duration.getImpl() == _duration.getImpl();
  }
};

// Implement hashing necessary for unordered_map<IntervalVarKey, ..>.
namespace std {
  template<> struct hash<IntervalVarKey> {
    std::size_t operator()(IntervalVarKey const& key) const noexcept {
      std::size_t h1 = std::hash<const void*>{}(key.getStartImpl());
      std::size_t h2 = std::hash<const void*>{}(key.getDurationImpl());
      return (h1*43 + h2) % 1000000007;
    }
  };
}

// Additional information we keep about each defined variable:
struct DefinedVarInfo {
  Location _location;      // Location of the "is_defined_var" annotation
  bool _alreadyDefined;    // Whether we already found a constraint with "defined_var" annotation
  DefinedVarInfo(const Location& location):
    _location(location),
    _alreadyDefined(false)
  {}
};

typedef std::unordered_map<IntervalVarKey, cpoptimizer::IntervalVar> IntervalVarMap;
typedef std::unordered_map<cpoptimizer::Expr, DefinedVarInfo> DefinedVarMap;
typedef std::unordered_set<cpoptimizer::Expr> ExprSet;

class FznReader {
 private:
  cpoptimizer::CP       _cp;
  cpoptimizer::Model    _model;
  const char*           _filename;
  std::istream&         _input;
  DefinedVarMap         _definedVariables;    // For variables with "is_defined_var" gives additional information
  OutputSpecification   _outputSpecification; // Will be returned by getOutputSpecification using std::move
  IntervalVarMap        _intervalVarMap;
  bool                  _hadError;
  ExprSet               _possiblyUnused;      // Possibly unused expressions that could be deleted once the whole file was read

 private:
  inline cpoptimizer::IntervalVar getIntervalVarFor(cpoptimizer::IntExpr start, cpoptimizer::IntExpr duration);

  // There's a constraint x==y in the model. If (1) x is defined variable and
  // (2) the constraint is marked to define x then try to set mutable for x to
  // y. Returns true if successful (to signalize whether equality x==y is
  // needed). Note that the case y==x is handled outside this function by
  // exchanging arguments x and y.
  // There are two template arguments since it is possible to assign bool value
  // to an integer defined variable.
  template <class ExprTypeX, class ExprTypeY>
  bool tryToDefineVar(ExprTypeX x, ExprTypeY y, ConstraintAnnotations* annotations) {
    if (!annotations->_definedVar)
      return false;
    if (annotations->_definedVar->getImpl() != x.getImpl()) {
      error(annotations->_definesVarLoc, "The constraint cannot define the specified variable.");
      return false;
    }
    DefinedVarMap::iterator infoIter = _definedVariables.find(x);
    if (infoIter == _definedVariables.end()) {
      warning(annotations->_definesVarLoc, "The specified variable was not marked as defined.");
      return false;
    }
    DefinedVarInfo& info = infoIter->second;
    if (info._alreadyDefined) {
      warning(annotations->_definesVarLoc, "There are two constraints marked as defining the same variable.");
      return false;
    }
    typename ExprTypeX::Mutable mutableExpr = *x.template downcast<typename ExprTypeX::Mutable>();
    assert(!mutableExpr.isDefined());
    mutableExpr.set(y);
    info._alreadyDefined = true;
    return true;
  }

  // Recursive variadic templates for display error and warning message. The
  // message is first printed into temporary stringstream so that we can use
  // default C locale.
  // Fallback function for the case of only one argument (ends the recursion):
  template <class T>
  static void formatMsgRecursion(std::stringstream& buffer, T v) {
    buffer << v;
  }
  // Print the first argument and do recursive call to print the rest:
  template <typename T, typename... Ts>
  static void formatMsgRecursion(std::stringstream& buffer, T head, Ts... tail) {
    buffer << head;
    formatMsgRecursion(buffer, tail...);
  }
  // Sometimes the initial call can be without arguments:
  void formatMsgRecursion(std::stringstream&) {
  }
  // Test whether argument is a location. If yes print it, otherwise do nothing.
  void printLocation(std::stringstream& buffer, Location l) {
    // Yes, argument is a location
    buffer << _filename << ":" << l._line << ":" << l._firstColumn << ": ";
  }
  template <typename T>
  void printLocation(std::stringstream&, T) {
    // No, argument is not a location
  }
  // formatMsgSkipLocation prints firstArgument is not Location. Because if it is Location then it was already printed.
  template <typename... Ts>
  void formatMsgSkipLocation(std::stringstream& buffer, Location /*firstArgument*/, Ts... args) {
    // Don't print firstArgument:
    formatMsgRecursion(buffer, args...);
  }
  template <typename T, typename... Ts>
  void formatMsgSkipLocation(std::stringstream& buffer, T firstArgument, Ts... args) {
    // Print also firstArgument:
    formatMsgRecursion(buffer, firstArgument, args...);
  }
  // The main warning/error formatting function
  template <bool isError, typename T, typename... Ts>
  void formatMsg(T firstArgument, Ts... args) {
    // Create and initialize buffer:
    std::stringstream buffer;
    buffer.imbue(std::locale::classic());
    // If firstArgument is location then print it, otherwise do nothing:
    printLocation(buffer, firstArgument);
    // Print warning/error header and remember if we had an error:
    if (isError) {
      _hadError = true;
      buffer << "Error: ";
    } else
      buffer << "Warning: ";
    // Print the remaining argument but skip firstArgument if it is a location.
    // We are going to use function overloading to choose the right function
    // depending on type of variable 'aux' below:
    formatMsgSkipLocation(buffer, firstArgument, args...);
    // Finally output the buffer:
    std::cerr << buffer.str();
    // Do not flush the stream in case of warning:
    if (isError)
      std::cerr << std::endl;
    else
      std::cerr << '\n';
  }

 public:
  FznReader(cpoptimizer::CP cp, std::ifstream& stream, const char* filename);

  cpoptimizer::Model getModel() const { return _model; }

  // TODO/3: Sometimes number of errors/warnings is huge and can slow us down.
  //         Introduce an option to limit them?
  template <typename... Ts>
  void error(Ts... args) {
    formatMsg<true>(args...);
  }
  template <typename... Ts>
  void warning(Ts... args) {
    formatMsg<false>( args...);
  }

  void setLocation(cpoptimizer::Expr expr, const Location& location) {
    expr.setLocation(_filename, location._line);
  }

  // Provide input to flex:
  int readInput(char* buf, size_t max) {
    (void)_input.read(buf, max);
    if (_input.bad())
      return -1;
    else
      return (int)_input.gcount();
  }
  // For flex: try to find symbol in symbol table, fill semantics appropriately
  // and return corresponding token:
  int decodeSymbol(const char* symbol, FznSemantics* semantics);

  void markDefinedVar(cpoptimizer::FloatExpr var, const Location& annotationLocation) {
    assert(_definedVariables.find(var) == _definedVariables.end());
    _definedVariables.emplace(var, annotationLocation);
  }

  cpoptimizer::IntExprArray addPrefix(IlcInt prefix, cpoptimizer::IntExprArray src) {
    const IlcInt n = src.getSize();
    IntExprVector exprs;
    exprs.reserve(n+1);
    exprs.push_back(_model.intExpr(prefix));
    for (IlcInt i = 0; i < n; i++)
      exprs.push_back(src[i]);
    return _model.intExprArray(exprs);
  }

  cpoptimizer::IntExprArray addSuffix(cpoptimizer::IntExprArray src, IlcInt suffix) {
    const IlcInt n = src.getSize();
    IntExprVector exprs;
    exprs.reserve(n+1);
    for (IlcInt i = 0; i < n; i++)
      exprs.push_back(src[i]);
    exprs.push_back(_model.intExpr(suffix));
    return _model.intExprArray(exprs);
  }

  cpoptimizer::IntExprArray incrArrayItems(cpoptimizer::IntExprArray src, IlcInt k) {
    const IlcInt n = src.getSize();
    IntExprVector exprs;
    exprs.reserve(n);
    for (IlcInt i = 0; i < n; i++)
      exprs.push_back(src[i]+k);
    return _model.intExprArray(exprs);
  }

  // We can add constraints and objective, so we use template:
  void add(cpoptimizer::Constraint constraint, const Location& constraintLoc, ConstraintAnnotations* annotations) {
    if (annotations->_definedVar)
      error(annotations->_definesVarLoc, "The constraint cannot define the specified variable.");
    setLocation(constraint, constraintLoc);
    _model.add(constraint);
    delete annotations;
  }
  void addObjective(cpoptimizer::Objective objective, const Location& location) {
    setLocation(objective, location);
    _model.add(objective);
  }
  void addName(cpoptimizer::Expr expr, const char* name, const Location& loc) {
    expr.name(name);
    setLocation(expr, loc);
  }

  // The following function assign is used by most flatzinc functions to say
  // that x == y where x is a variable (possibly defined variable) and y is an expression.
  // Note: it is not used for the case when y is also a variable.
  // Note: The function takes two template arguments since it is possible to
  // assign boolean expression to an integer variable (not in flatzinc, but in
  // CP Optimizer, and we use that during translation).
  template <class ExprTypeX, class ExprTypeY>
  void assign(ExprTypeX x, ExprTypeY y, const Location& location, ConstraintAnnotations* annotations) {
    if (tryToDefineVar(x, y, annotations))
      setLocation(y, location);
    else {
      cpoptimizer::Constraint equality = _model.add(x == y);
      setLocation(equality, location);
    }
    delete annotations;
  }

  // Handle flatzinc constraint x==y. It can defined y or x.
  // Equality is defined only between expressions of the same type, therefore
  // only one template argument.
  template <class ExprType>
  void equal(ExprType x, ExprType y, const Location& location, ConstraintAnnotations* annotations) {
    bool done;
    if (annotations->_definedVar && annotations->_definedVar->getImpl() == x.getImpl())
      done = tryToDefineVar(x, y, annotations);
    else
      done = tryToDefineVar(y, x, annotations);
    if (!done) {
      // Location is assign only if it is not definition of defined var.
      // Because otherwise there's no place to put the location (we assume that
      // y already have a location).
      cpoptimizer::Constraint equality = _model.add(x == y);
      setLocation(equality, location);
    }
    delete annotations;
  }

  // Handle flatzinc constraint x + y == z. It is a template because there are
  // integer and floating-point versions. Depending on the annotation this
  // constraint can define x, y or z so we may need to rewrite the constraint.
  template <class ExprType>
  void plus(ExprType x, ExprType y, ExprType z, const Location& location, ConstraintAnnotations* annotation) {
    if (annotation->_definedVar && annotation->_definedVar->getImpl() == x.getImpl())
      assign(x, z-y, location, annotation);
    else if (annotation->_definedVar && annotation->_definedVar->getImpl() == y.getImpl())
      assign(y, z-x, location, annotation);
    else
      assign(z, x+y, location, annotation);
  }

  // bool_array_xor may actually define one of its variables. In this case we
  // need to rewrite the expressions.
  void assignFromArrayXor(cpoptimizer::BoolExprArray params, const Location& location, ConstraintAnnotations* annotation);

  bool verifyVarArrayParams(IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, VarAnnotations* annotations);

  bool verifyArrayRange(IlcInt minIndex, const Location& minIndexLocation,
                        IlcInt maxIndex, const Location& maxIndexLocation,
                        IlcInt arraySize)
  {
    if (minIndex != 1) {
      error(minIndexLocation, "Minimum index of an array must be 1, not minIndex.");
      return false;
    }
    if (maxIndex != arraySize) {
      error(maxIndexLocation, "Array size does not match (", maxIndex, " versus ", arraySize, ").");
      return false;
    }
    return true;
  }

  bool verifyArrayIndex(IlcInt index, const Location& indexLocation, IlcInt arraySize) {
    // In MiniZinc, arrays are indexed from 1.
    if (index > arraySize || index < 1) {
      error(indexLocation, "Array index ", index, " is outside of array range 1..", arraySize, ".");
      return false;
    }
    return true;
  }

  void addOutput(cpoptimizer::FloatExpr var, const char* name) {
    _outputSpecification.addLine(name, var);
  }

  void addOutput(cpoptimizer::Expr array, const char* name, VarAnnotations* annotations) {
    assert(annotations->_outputRanges);
    _outputSpecification.addLine(name, array, annotations->_outputRanges.release());
  }

  cpoptimizer::IntExpr createVar(const char* name, Domain* domain, Location location, const VarAnnotations* annotations);
  void createVar(const char* name, Domain* domain, Location varLocation, const VarAnnotations* annotations, IlcInt value, Location eqLocation);

  // Create new array of variables with given domain:
  void createVarArray(const char* name, Domain* domain, IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, VarAnnotations* annotations);
  // Put existing variables into a new array:
  void createVarArray(const char* name, Domain* domain, IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, cpoptimizer::IntExprArray items, VarAnnotations* annotations);

  static void deleteVarArgs(Domain* domain, char* identifier, VarAnnotations* annotations) {
    delete domain;
    free(identifier);
    delete(annotations);
  }

  // The template function is explicitly instantiated in the cpp file.
  template <typename CoefArray, typename ExprArray>
  void assignFromLinEq(CoefArray srcCoefs, ExprArray srcExprs, typename CoefArray::ItemType val, const Location& loc, ConstraintAnnotations* annotations);

  cpoptimizer::BoolExpr tableConstraint(cpoptimizer::IntExprArray vars, cpoptimizer::IntArray table, const Location& loc);
  cpoptimizer::Constraint cumulative(cpoptimizer::IntExprArray starts, cpoptimizer::IntExprArray durations, cpoptimizer::IntExprArray capacities, cpoptimizer::IntExpr capacityMax, const Location& loc);

  // bool_clause is a constraint saying that at least one bool vars from
  // "positives" must be true or at least one of the bool vars in "negatives"
  // must be false.
  cpoptimizer::BoolExpr clause(cpoptimizer::BoolExprArray positives, cpoptimizer::BoolExprArray negatives) {
    BoolExprVector literals;
    literals.reserve(positives.getSize() + negatives.getSize());
    for (cpoptimizer::BoolExpr expr: positives)
      literals.push_back(expr);
    for (cpoptimizer::BoolExpr expr: negatives)
      literals.push_back(!expr);
    return _model.disjunction(literals);
  }

  cpoptimizer::Constraint packFixedCaps(cpoptimizer::IntArray cap, cpoptimizer::IntExprArray where, cpoptimizer::IntArray weight) {
    const IlcInt m = cap.getSize();
    IntExprVector exprs;
    exprs.reserve(m+1);
    // Minizinc counts bin from 1, CP Optimizer from 0. So add bin 0 with capacity 0:
    exprs.push_back(_model.intExpr(0));
    // For each bin create a variable with the load:
    for (IlcInt c: cap)
      exprs.push_back(_model.intVar().setDomain(0, c));
    return _model.pack(exprs, where, weight);
  }

  cpoptimizer::IntExpr intPowFixed(cpoptimizer::IntExpr x, IlcInt exponent, const Location& location);

  // The main function: parse given file and return true in case of success.
  bool read();
  // Return output specification after successful read:
  OutputSpecification && getOutputSpecification() {
    return std::move(_outputSpecification);
  }
};

ILCGCCHIDINGOFF

#endif
