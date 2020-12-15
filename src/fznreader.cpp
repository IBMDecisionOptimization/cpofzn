// --------------------------------------------------------------------------
// Source file provided under Apache License, Version 2.0, January 2004,
// http://www.apache.org/licenses/
// (c) Copyright IBM Corp. 2019, 2020
// --------------------------------------------------------------------------

#include "fznreader.h"
#include "fznparser.h"

// Do not include unistd.h from fznscanner.h:
#define YY_NO_UNISTD_H 1
#include "fznscanner.h"

ILCGCCHIDINGON

using namespace cpoptimizer;

static void PrintIncumbentValue(CP cp, FloatExpr expr) {
  IlcFloat value = cp.getIncumbentValue(expr);
  if (expr.downcast<BoolExpr>())
    std::cout << (value ? "true" : "false");
  else if (expr.downcast<IntExpr>())
    std::cout << (IlcInt)value;
  else
    std::cout << value;
}

void OutputLine::print(CP cp) {
  std::cout << _name << " = ";
  if (!_ranges) {
    PrintIncumbentValue(cp, *_expr.downcast<FloatExpr>());
    std::cout << ";\n";
    return;
  }
  FloatExprArray array = *_expr.downcast<FloatExprArray>();
  std::cout << "array" << _ranges->size() << "d(";
  bool first = true;
  for (IntRange r: *_ranges) {
    if (!first)
      std::cout << ", ";
    else
      first = false;
    std::cout << r._min << ".." << r._max;
  }
  std::cout << ", [";
  first = true;
  for (FloatExpr item: array) {
    if (!first)
      std::cout << ", ";
    else
      first = false;
    PrintIncumbentValue(cp, item);
  }
  std::cout << "]);\n";
}

template <typename CoefArray, typename ExprArray>
void FznReader::assignFromLinEq(CoefArray srcCoefs, ExprArray srcExprs, typename CoefArray::ItemType val, const Location& loc, ConstraintAnnotations* annotations) {
  if (!annotations->_definedVar) {
    // We can create the constraint as it is:
    add(_model.scalProd(srcCoefs, srcExprs) == val, loc, annotations);
    return;
  }
  // The linear equation defines one of the member variables. However the constraint is:
  //   val == scalProd(...)
  // We are going to rewrite it into:
  //   definedVar == scalProd(....) - val
  const IlcInt n = srcCoefs.getSize();
  if (n != srcExprs.getSize()) {
    error(loc, "Sizes of arrays do not match (sizes are ", srcExprs.getSize(), " and ", srcCoefs.getSize(), ").");
    return;
  }
  // Find position of definedVar in source scalProd, copy the rest into vectors:
  IlcInt pos = -1;
  std::vector<typename CoefArray::ItemType> coefs;
  coefs.reserve(n);
  std::vector<typename ExprArray::ItemType> exprs;
  exprs.reserve(n);
  for (IlcInt i = 0; i < n; i++) {
    if (srcExprs[i].getImpl() == annotations->_definedVar->getImpl()) {
      if (pos != -1)
        pos = -2; // We found the variable more than once
      else
        pos = i;
    } else {
      coefs.push_back(srcCoefs[i]);
      exprs.push_back(srcExprs[i]);
    }
  }
  if (pos < 0 || (srcCoefs[pos] != 1 && srcCoefs[pos] != -1)) {
    // Note that function add bellow will complain that it cannot define the
    // specified variable. So we don't have to do warning ourselves.
    add(_model.scalProd(srcCoefs, srcExprs) == val, loc, annotations);
    return;
  }
  // We are not going to use srcCoefs and srcExprs directly. They can be
  // deleted if they are not used otherwise in the file:
  _possiblyUnused.insert(srcCoefs);
  _possiblyUnused.insert(srcExprs);
  typename ExprArray::ItemType definedVar = srcExprs[pos];
  // The defined definedVar can appear only once and only with coefficient +/-1).
  if (srcCoefs[pos] == 1) {
    // Invert sign of coefficients since we are going to put
    val = -val;
    for (auto& v: coefs)
      v = -v;
  }
  // Type of the result depends on template parameters, so we use auto:
  auto result = _model.scalProd(coefs, exprs);
  if (val != 0)
    result = (result - val);
  assign(definedVar, result, loc, annotations);
}

// Explicit instantiations of the template function above:
template void FznReader::assignFromLinEq<IntArray,   IntExprArray>  (IntArray   srcCoefs, IntExprArray   srcExprs, IlcInt   val, const Location& loc, ConstraintAnnotations* annotations);
template void FznReader::assignFromLinEq<FloatArray, FloatExprArray>(FloatArray srcCoefs, FloatExprArray srcExprs, IlcFloat val, const Location& loc, ConstraintAnnotations* annotations);

void FznReader::assignFromArrayXor(cpoptimizer::BoolExprArray params, const Location& location, ConstraintAnnotations* annotations) {
  if (annotations->_definedVar) {
    OptBoolExpr defined;
    BoolExprVector others;
    others.reserve(params.getSize());
    for (BoolExpr p: params) {
      if (p.getImpl() == annotations->_definedVar->getImpl())
        defined = p;
      else
        others.push_back(p);
    }
    if (defined) {
      assign(*defined, _model.sum(others) % 2 == 0, location, annotations);
      return;
    } else
      warning(annotations->_definesVarLoc, "Warning: array_bool_xor cannot define specified variable.");
  }
  add(_model.sum(params) % 2 == 1, location, annotations);
}

IntExpr FznReader::createVar(const char* name, Domain* domain, Location location, const VarAnnotations* annotations) {
  OptIntExpr result;
  if (domain->isBoolean()) {
    assert(domain->getIntMin() == 0);
    assert(domain->getIntMax() == 1);
    if (annotations->_isDefinedVar)
      result = _model.mutableBoolExpr();
    else
      result = _model.boolVar();
  } else {
    if (annotations->_isDefinedVar) {
      result = _model.mutableIntExpr();
      if (domain->isEnumerated())
        _model.add(_model.allowedAssignments(*result, domain->getEnumeratedValues()));
      else
        _model.add(_model.range(*result, domain->getIntMin(), domain->getIntMax()));
    } else {
      IntVar var = _model.intVar();
      if (domain->isEnumerated())
        var.setDomain(domain->getEnumeratedValues());
      else
        var.setDomain(domain->getIntMin(), domain->getIntMax());
      result = var;
    }
  }
  result->name(name);
  setLocation(*result, location);
  if (annotations->_isDefinedVar)
    markDefinedVar(*result, annotations->_definedLocation);
  if (annotations->_isOutputVar)
    addOutput(*result, name);
  return *result;
}

void FznReader::createVar(const char* name, Domain* domain, Location varLocation, const VarAnnotations* annotations, IlcInt value, Location eqLocation) {
  IntExpr result = createVar(name, domain, varLocation, annotations);
  Constraint equality = (result == value);
  setLocation(equality, eqLocation);
  _model.add(equality);
}

bool FznReader::verifyVarArrayParams(IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, VarAnnotations* annotations) {
  if (minIndex != 1) {
    error(varLocation, "Flatzinc arrays must start by index 1.");
    return false;
  }
  if (maxIndex < 0) {
    error(varLocation, "Array of negative size.");
    return false;
  }
  if (annotations->_isDefinedVar)
    warning(varLocation, "Annotation defined_var cannot be used on arrays.");
  return true;
}

void FznReader::createVarArray(const char* name, Domain* domain, IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, VarAnnotations* annotations) {
  if (!verifyVarArrayParams(minIndex, maxIndex, varLocation, annotations))
    return;
  OptIntExprArray varArray;
  if (domain->isBoolean()) {
    assert(domain->getIntMin() == 0);
    assert(domain->getIntMax() == 1);
    BoolVarVector vars = _model.boolVarVector(maxIndex);
    NameItems(vars, name, 1);
    varArray = _model.boolExprArray(vars);
  } else {
    IntVarVector vars = _model.intVarVector(maxIndex);
    NameItems(vars, name, 1);
    if (!domain->isEnumerated())
      SetDomain(vars, domain->getIntMin(), domain->getIntMax());
    else
      SetDomain(vars, domain->getEnumeratedValues());
    varArray = _model.intExprArray(vars);
  }
  varArray->name(name);
  setLocation(*varArray, varLocation);
  if (annotations->_isOutputVar)
    addOutput(*varArray, name, annotations);
}

void FznReader::createVarArray(const char* name, Domain* domain, IlcInt minIndex, IlcInt maxIndex, const Location& varLocation, IntExprArray items, VarAnnotations* annotations) {
  if (!verifyVarArrayParams(minIndex, maxIndex, varLocation, annotations))
    return;
  if (items.getSize() != maxIndex) {
    error(varLocation, "Aliased variable array has wrong size ", items.getSize(), " (expected ", maxIndex, ").");
    return;
  }
  if (domain->isBoolean()) {
    assert(domain->getIntMin() == 0);
    assert(domain->getIntMax() == 1);
    if (!items.downcast<BoolExprArray>()) {
      error(varLocation, "Supplied variables are not boolean.");
      return;
    }
  } else {
    if (domain->isEnumerated())
      for (IntExpr expr: items)
        _model.add(_model.allowedAssignments(expr, domain->getEnumeratedValues()));
    else if (domain->getIntMin() != IlcIntMin && domain->getIntMax() != IlcIntMax)
      for (IntExpr expr: items)
        _model.add(_model.range(expr, domain->getIntMin(), domain->getIntMax()));
  }
  addName(items, name, varLocation);
  if (annotations->_isOutputVar)
    addOutput(items, name, annotations);
}

inline IntervalVar FznReader::getIntervalVarFor(IntExpr start, IntExpr duration) {
  IntervalVarKey key(start, duration);
  // Try to find the interval if between already created intervals
  auto found = _intervalVarMap.find(key);
  if (found != _intervalVarMap.end())
    return found->second;
  // TODO/2: We cannot name the interval variable because we don't have getName.
  IntervalVar itv = _model.intervalVar();
  _model.add(_model.startOf(itv) == start);
  _model.add(_model.lengthOf(itv) == duration);
  _intervalVarMap.insert({key, itv});
  return itv;
}

Constraint FznReader::cumulative(IntExprArray starts, IntExprArray durations, IntExprArray capacities, IntExpr capacityMax, const Location& loc) {
  const IlcInt n = starts.getSize();
  if (durations.getSize() != n || capacities.getSize() != n) {
    error(loc, "Array arguments for cumulative constraints have different sizes: ", n, ", ", durations.getSize(), ", ", capacities.getSize(), ".");
    return _model.boolExpr(false);
  }
  std::vector<CumulExpr> cumul;
  cumul.reserve(n);
  for (IlcInt i = 0; i < n; i++) {
    IntervalVar interval = getIntervalVarFor(starts[i], durations[i]);
    IntExpr c = capacities[i];
    if (OptIntConstant constant = c.downcast<IntConstant>())
      cumul.push_back(_model.pulse(interval, constant->getValue()));
    else {
      CumulExpr pulse = _model.pulse(interval, 0, IloIntervalMax);
      _model.add(_model.heightAtStart(interval, pulse) == c);
      cumul.push_back(pulse);
    }
  }
  return _model.sum(cumul) <= capacityMax;
}

BoolExpr FznReader::tableConstraint(IntExprArray vars, IntArray table, const Location& loc) {
  const IlcInt n = vars.getSize();
  if (n == 0)
    return _model.boolExpr(true);
  if (n == 1)
    return _model.allowedAssignments(vars[0], table);
  if (table.getSize() % n) {
    error(loc, "Array arguments for table constraint have incompatible sizes: ", vars.getSize(), ", ", table.getSize(), ".");
    return _model.boolExpr(false);
  }
  const IlcInt m = table.getSize() / n;
  IntVector line;
  line.reserve(n);
  std::vector<IntArray> tuples;
  tuples.reserve(m);
  IlcInt pos = 0;
  for (IlcInt j = 0; j < m; j++) {
    for (IlcInt i = 0; i < n; i++)
      line.push_back(table[pos++]);
    tuples.push_back(_model.intArray(line));
    line.clear();
  }
  return _model.allowedAssignments(vars, tuples);
}

// For flatzinc function int_pow_fixed: compute x^exponent:
IntExpr FznReader::intPowFixed(IntExpr x, IlcInt exponent, const Location& location) {
  if (exponent < 0) {
    error(location, "Function int_pow_fixed cannot be used with negative exponent.");
    return _model.intExpr(0);
  }
  if (exponent == 1)
    return _model.intExpr(1);
  if (exponent == 2)
    return _model.square(x);
  IntExpr result = x;
  for (IlcInt i = 1; i < exponent; i++)
    result = result*x;
  return result;
}

int FznReader::decodeSymbol(const char* symbol, FznSemantics* semantics) {
  OptExpr expr = getModel().getExprByName(symbol);
  if (!expr) {
    semantics->_string = strdup(symbol);
    return NEW_IDENT;
  }
  switch (expr->getType()) {
    case Expr::BoolVarT:
    case Expr::MutableBoolExprT:
      semantics->_boolExpr = *expr->downcast<BoolExpr>();
      return BOOL_VAR;
    case Expr::BoolConstantT:
      semantics->_boolExpr = *expr->downcast<BoolConstant>();
      return BOOL_CONSTANT;
    case Expr::IntVarT:
    case Expr::MutableIntExprT:
      semantics->_intExpr = *expr->downcast<IntExpr>();
      return INT_VAR;
    case Expr::IntConstantT:
      semantics->_intExpr = *expr->downcast<IntConstant>();
      return INT_CONSTANT;
    case Expr::FloatConstantT:
      semantics->_floatExpr = *expr->downcast<FloatConstant>();
      return FLOAT_CONSTANT;
      // TODO/2: Expr::FloatVarT doesn't exist yet
    case Expr::MutableFloatExprT:
      semantics->_floatExpr = *expr->downcast<FloatExpr>();
      return FLOAT_VAR;
    case Expr::IntArrayT: {
      IntArray result = *expr->downcast<IntArray>();
      // CP Optimizer does not have BoolArray. So we use IntArray instead. And
      // when we find IntArray by symbol then we have to test whether it is just
      // 0/1 array. It has a side effect: user may declare 0/1 INT_ARRAY and we
      // will turn it into BOOL_ARRAY. But it is not a problem, we allow
      // BOOL_ARRAY everywhere where INT_ARRAY is expected.
      for (IlcInt v: result)
        if (v != 0 && v != 1) {
          semantics->_intArray = result;
          return INT_ARRAY;
        }
      semantics->_boolArray = result;
      return BOOL_ARRAY;
    }
    case Expr::FloatArrayT:
      semantics->_floatArray = *expr->downcast<FloatArray>();
      return FLOAT_ARRAY;
    case Expr::BoolExprArrayT:
      semantics->_boolVarArray = *expr->downcast<BoolExprArray>();
      return BOOL_VAR_ARRAY;
    case Expr::IntExprArrayT:
      semantics->_intVarArray = *expr->downcast<IntExprArray>();
      return INT_VAR_ARRAY;
    case Expr::FloatExprArrayT:
      semantics->_floatVarArray = *expr->downcast<FloatExprArray>();
      return FLOAT_VAR_ARRAY;
    case Expr::IntExprT:
    case Expr::FloatExprT:
    case Expr::BoolExprT:
      // Flatzinc doesn't have expressions.
      // Therefore only (int/bool/float) variables or (int/bool/float) constants can be found by name:
    default:
      assert(false);
  }
  return 0; // To get rid of warnings
}

// The following class makes sure that FznParse_lex_destroy is always called
// even if an exception is thrown.
class FznScannerGuard {
 private:
  void* _scanner;
 public:
  FznScannerGuard(void* scanner): _scanner(scanner) {}
  int destroy() {
    assert(_scanner);
    int retVal = FznParse_lex_destroy(_scanner);
    _scanner = nullptr;
    return retVal;
  }
  ~FznScannerGuard() {
    if (_scanner)
      FznParse_lex_destroy(_scanner);
  }
};

bool FznReader::read() {
  void* scanner;
  int status = FznParse_lex_init_extra(this, &scanner);
  FznScannerGuard flexLock(scanner);
  if (status) {
    error("System error: ", strerror(errno));
    return false;
  }

  status = FznParse_parse(scanner, this);
  if (status == 1) {
    // There was some syntax error
    return false;
  }
  if (status == 2) {
    error("Not enough memory to read the file '", _filename, "'.");
    return false;
  }
  if (status) {
    error("Unrecognized error has occured.");
    return false;
  }

  status = flexLock.destroy();
  if (status) {
    error("System error: ", strerror(errno));
    return false;
  }

  if (_hadError)
    return false;

  // Test whether all variables with is_defined_var are indeed defined
  for (auto& definedVar: _definedVariables) {
    Expr var = definedVar.first;
    DefinedVarInfo& info = definedVar.second;
    if (!info._alreadyDefined) {
      warning(info._location, "Variable is marked as defined_var, however no definition was provided.");
      if (OptMutableIntExpr intMutable = var.downcast<MutableIntExpr>()) {
        assert(!intMutable->isDefined());
        intMutable->set(_model.intVar());
      } else {
        OptMutableBoolExpr boolMutable = var.downcast<MutableBoolExpr>();
        assert(boolMutable);
        assert(!boolMutable->isDefined());
        boolMutable->set(_model.boolVar());
      }
    }
  }

  // Remove possibly unused expressions
  for (Expr e: _possiblyUnused)
    _model.dispose(e);
  _possiblyUnused.clear();

  _cp.hackBeforeSolve();

  return true;
}

FznReader::FznReader(CP cp, std::ifstream& stream, const char* filename):
  _cp(cp),
  _model(Model(_cp)),
  _filename(filename),
  _input(stream),
  _hadError(false)
{}
