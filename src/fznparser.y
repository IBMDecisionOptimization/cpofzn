// --------------------------------------------------------------------------
// Source file provided under Apache License, Version 2.0, January 2004,
// http://www.apache.org/licenses/
// (c) Copyright IBM Corp. 2019, 2020
//
// This code is partly based on earlier contributions from Nick Nethercote
// and Julien Fischer. These contributions are in the public domain.
// --------------------------------------------------------------------------


// More verbose error messages than just "syntax error":
%error-verbose
// Change standard yy prefix to FznParse_
%name-prefix "FznParse_"
// Generate code to track current location:
%locations
// Generate reentrant parser:
%define api.pure
// Look ahead auto correction (better error messages, some slowdown):
%define parse.lac full
// Additional parameters passed to the yyparse call. Scanner is required for reentrant flex scanner.
%parse-param{void* scanner}
%parse-param{class FznReader* reader}
// Additional parameter passed to flex scanner function:
%lex-param{void* scanner}

%{
#include "fznreader.h"

ILCGCCHIDINGON

#define MODEL     (reader->getModel())

struct YYLTYPE;

// Defined by flex:
int ILCHIDDEN FznParse_lex(YYSTYPE * yylval_param, YYLTYPE * yylloc_param, void* yyscanner);
// Required by bison:
static void yyerror(YYLTYPE* location, void* scanner, FznReader* reader, const char* msg);

/* Default action for computation of a location of a non-terminal. The rule
 * computes location "Current" of a rule that just matched (grouped) N
 * right-hand side elements. We are interested only where the rule starts, not
 * where it ends. Therefore field _lastColumn is not computed (it is used only
 * by flex). */
#define YYLLOC_DEFAULT(Current, Rhs, N)                             \
    do {                                                            \
      if (N)                                                        \
        {                                                           \
           (Current)._line        = YYRHSLOC(Rhs, 1)._line;         \
           (Current)._firstColumn = YYRHSLOC(Rhs, 1)._firstColumn;  \
        }                                                           \
      else                                                          \
        {                                                           \
           (Current)._line        = YYRHSLOC(Rhs, 0)._line;         \
           (Current)._firstColumn = YYRHSLOC(Rhs, 0)._lastColumn;   \
        }                                                           \
    } while (0)

%}

%initial-action {
  // Initialize first location
  @$._line = 1;
  @$._firstColumn = @$._lastColumn = 0;
}

// Token kinds
%token  <_int>             INT_LITERAL     "integer value"
%token  <_int>             BOOL_LITERAL    "boolean value"
%token  <_string>          NEW_IDENT       "uknown identifier"
%token  <_float>           FLOAT_LITERAL   "float value"
%token  <_boolExpr>        BOOL_VAR        "boolean variable"
%token  <_intExpr>         INT_VAR         "integer variable"
%token  <_floatExpr>       FLOAT_VAR       "float variable"
%token  <_intArray>        INT_ARRAY       "integer array"
%token  <_boolArray>       BOOL_ARRAY      "boolean array"
%token  <_floatArray>      FLOAT_ARRAY     "float array"
%token  <_boolVarArray>    BOOL_VAR_ARRAY  "bool var array"
%token  <_intVarArray>     INT_VAR_ARRAY   "integer variable array"
%token  <_floatVarArray>   FLOAT_VAR_ARRAY "float variable array"
%token  <_boolConstant>    BOOL_CONSTANT   "boolean constant"
%token  <_intConstant>     INT_CONSTANT    "integer constant"
%token  <_floatConstant>   FLOAT_CONSTANT  "float constant"

// String literal is actually not used in the grammar so it is commented out:
// %token  <_string>          STRING_LITERAL  "string"

%token  ARRAY       "array"
%token  BOOL        "bool"
%token  CONSTRAINT  "constraint"
%token  FLOAT       "float"
%token  INT         "int"
%token  MAXIMIZE    "maximize"
%token  MINIMIZE    "minimize"
%token  OF          "of"
%token  PREDICATE   "predicate"
%token  SATISFY     "satisfy"
%token  SET         "set"
%token  SOLVE       "solve"
%token  VAR         "var"
%token  COLONCOLON  "::"
%token  DOTDOT      ".."

// Integer builtins:
%token  ARRAY_INT_ELEMENT        "array_int_element"
%token  ARRAY_VAR_INT_ELEMENT    "array_var_int_element"
%token  INT_ABS                  "int_abs"
%token  INT_DIV                  "int_div"
%token  INT_EQ                   "int_eq"
%token  INT_EQ_REIF              "int_eq_reif"
%token  INT_LE                   "int_le"
%token  INT_LE_REIF              "int_le_reif"
%token  INT_LIN_EQ               "int_lin_eq"
%token  INT_LIN_EQ_REIF          "int_lin_eq_reif"
%token  INT_LIN_LE               "int_lin_le"
%token  INT_LIN_LE_REIF          "int_lin_le_reif"
%token  INT_LIN_NE               "int_lin_ne"
%token  INT_LIN_NE_REIF          "int_lin_ne_reif"
%token  INT_LT                   "int_lt"
%token  INT_LT_REIF              "int_lt_reif"
%token  INT_CMAX                 "int_max"
%token  INT_CMIN                 "int_min"
%token  INT_MOD                  "int_mod"
%token  INT_NE                   "int_ne"
%token  INT_NE_REIF              "int_ne_reif"
%token  INT_PLUS                 "int_plus"
%token  INT_POW                  "int_pow"
%token  INT_POW_FIXED            "int_pow_fixed"
%token  INT_TIMES                "int_times"

// Boolean buitins:
%token  ARRAY_BOOL_AND           "array_bool_and"
%token  ARRAY_BOOL_ELEMENT       "array_bool_element"
%token  ARRAY_BOOL_OR            "array_bool_or"
%token  ARRAY_BOOL_XOR           "array_bool_xor"
%token  ARRAY_VAR_BOOL_ELEMENT   "array_var_bool_element"
%token  BOOL2INT                 "bool2int"
%token  BOOL_AND                 "bool_and"
%token  BOOL_CLAUSE              "bool_clause"
%token  BOOL_CLAUSE_REIF         "bool_clause_reif"
%token  BOOL_EQ                  "bool_eq"
%token  BOOL_EQ_REIF             "bool_eq_reif"
%token  BOOL_LE                  "bool_le"
%token  BOOL_LE_REIF             "bool_le_reif"
%token  BOOL_LIN_EQ              "bool_lin_eq"
%token  BOOL_LIN_LE              "bool_lin_le"
%token  BOOL_LT                  "bool_lt"
%token  BOOL_LT_REIF             "bool_lt_reif"
%token  BOOL_NOT                 "bool_not"
%token  BOOL_OR                  "bool_or"
%token  BOOL_XOR                 "bool_xor"

// Set builtins. Most of them are redefined in the mznlib.
%token  SET_IN                   "set_in"
%token  SET_IN_REIF              "set_in_reif"

// Float builtins:
%token  ARRAY_FLOAT_ELEMENT      "array_float_element"
%token  ARRAY_VAR_FLOAT_ELEMENT  "array_var_float_element"
%token  FLOAT_ABS                "float_abs"
%token  FLOAT_DIV                "float_div"
// TODO/2: Missing FLOAT_DOM, it is basically allowedAssignments with floats. It should go into redefinitions-2.1.mzn
%token  FLOAT_EQ                 "float_eq"
%token  FLOAT_EQ_REIF            "float_eq_reif"
%token  FLOAT_EXP                "float_exp"
%token  FLOAT_IN                 "float_in"
%token  FLOAT_IN_REIF            "float_in_reif"
%token  FLOAT_LE                 "float_le"
%token  FLOAT_LE_REIF            "float_le_reif"
%token  FLOAT_LIN_EQ             "float_lin_eq"
%token  FLOAT_LIN_EQ_REIF        "float_lin_eq_reif"
%token  FLOAT_LIN_LE             "float_lin_le"
%token  FLOAT_LIN_LE_REIF        "float_lin_le_reif"
%token  FLOAT_LIN_LT             "float_lin_lt"
%token  FLOAT_LIN_LT_REIF        "float_lin_lt_reif"
%token  FLOAT_LIN_NE             "float_lin_ne"
%token  FLOAT_LIN_NE_REIF        "float_lin_ne_reif"
%token  FLOAT_LN                 "float_ln"
%token  FLOAT_LOG10              "float_log10"
%token  FLOAT_LOG2               "float_log2"
%token  FLOAT_LT                 "float_lt"
%token  FLOAT_LT_REIF            "float_lt_reif"
%token  FLOAT_MAX                "float_max"
%token  FLOAT_MIN                "float_min"
%token  FLOAT_NE                 "float_ne"
%token  FLOAT_NE_REIF            "float_ne_reif"
%token  FLOAT_PLUS               "float_plus"
%token  FLOAT_POW                "float_pow"
%token  FLOAT_SQRT               "float_sqrt"
%token  INT_TO_FLOAT             "int_to_float"

// Supported global constraints:
%token ALL_DIFFERENT_INT       "fzn_all_different_int"
%token CPO_SUBCIRCUIT          "cpo_subcircuit"
%token COUNT_EQ_PAR            "fzn_count_eq_par"
%token CUMULATIVE              "fzn_cumulative"
%token CPO_INVERSE             "cpo_inverse"
%token LEX_LESSEQ_BOOL         "fzn_lex_lesseq_bool"
%token LEX_LESSEQ_INT          "fzn_lex_lesseq_int"
%token LEX_LESS_BOOL           "fzn_lex_less_bool"
%token LEX_LESS_INT            "fzn_lex_less_int"
%token ARRAY_FLOAT_MAXIMUM     "array_float_maximum"
%token ARRAY_INT_MAXIMUM       "array_int_maximum"
%token ARRAY_FLOAT_MINIMUM     "array_float_minimum"
%token ARRAY_INT_MINIMUM       "array_int_minimum"
%token TABLE_BOOL              "fzn_table_bool"
%token TABLE_BOOL_REIF         "fzn_table_bool_reif"
%token TABLE_INT               "fzn_table_int"
%token TABLE_INT_REIF          "fzn_table_int_reif"
%token BIN_PACKING_LOAD        "fzn_bin_packing_load"
%token BIN_PACKING_CAPA        "fzn_bin_packing_capa"
%token NVALUE                  "fzn_nvalue"

// TODO/2: 2.0.2 introduces array_var_bool_element_nonshifted and similar.
// However it is not clear to me what is the difference from var_bool_element?

// Annotations:
%token IS_DEFINED_VAR          "is_defined_var"
%token VAR_IS_INTRODUCED       "var_is_introduced"
%token OUTPUT_ARRAY            "output_array"
%token OUTPUT_VAR              "output_var"
%token DEFINES_VAR             "defines_var"
%token SEQ_SEARCH              "seq_search"
%token INT_SEARCH              "int_search"
%token BOOL_SEARCH             "bool_search"
%token COMPLETE                "complete"
%token FIRST_FAIL              "first_fail"
%token INPUT_ORDER             "input_order"
%token ANTI_FIRST_FAIL         "anti_first_fail"
%token SMALLEST                "smallest"
%token LARGEST                 "largest"
%token OCCURRENCE              "occurrence"
%token MOST_CONSTRAINED        "most_constrained"
%token MAX_REGRET              "max_regret"
%token INDOMAIN_MIN            "indomain_min"
%token INDOMAIN_MAX            "indomain_max"
%token INDOMAIN_MIDDLE         "indomain_middle"
%token INDOMAIN_MEDIAN         "indomain_median"
%token INDOMAIN                "indomain"
%token INDOMAIN_RANDOM         "indomain_random"
%token INDOMAIN_SPLIT          "indomain_split"
%token INDOMAIN_REVERSE_SPLIT  "indomain_reverse_split"
%token INDOMAIN_INTERVAL       "indomain_interval"


%type <_bool> constbool
%type <_int> constint
%type <_float> constfloat pureconstfloat
%type <_floatExpr> floatvar floatexpr constfloatexpr
%type <_intExpr> intvar intexpr
%type <_boolExpr> boolvar boolexpr
%type <_boolExpr> constboolexpr
%type <_intExpr> constintexpr
%type <_domain> domain explicitdomain
%type <_intList> boollist boollistnotempty intlist intlistnotempty
%type <_intArray> intarray boolarray
%type <_floatList> floatlist floatlistnotempty
%type <_floatArray> floatarray
%type <_boolVarList> boolexprlist boolexprlistnotempty
%type <_intVarList> intexprlist intexprlistnotempty
%type <_floatVarList> floatexprlist floatexprlistnotempty
%type <_intVarArray> intexprarray
%type <_boolVarArray> boolexprarray
%type <_floatVarArray> floatexprarray
%type <_constraintAnnotations> c_annots c_annot
%type <_variableAnnotations> var_annotations
%type <_intRange> outputrange
%type <_outputRanges> outputranges

// During error recovery parser discards symbols that are already in stack.
// But he needs to know what code to execute:
%destructor { delete $$; } <_domain>
%destructor { delete $$; } <_intList>
%destructor { delete $$; } <_floatList>
%destructor { delete $$; } <_boolVarList>
%destructor { delete $$; } <_intVarList>
%destructor { delete $$; } <_floatVarList>
%destructor { delete $$; } <_constraintAnnotations>
%destructor { delete $$; } <_variableAnnotations>
%destructor { delete $$; } <_outputRanges>
%%

//---------------------------------------------------------------------------
// Model top-level
//---------------------------------------------------------------------------

// Note: We want left-recursive rules since they consume much less stack.
// Left recursion: defined symbol appears as the leftmost symbol on the right hand side.

// Note: in order to avoid shift/reduce conflicts, rule for model handles in a
// special way the cases when parameters or variables are completely empty.
model:
    pred_decl_items parameters variables constraints model_end
  | pred_decl_items parameters           constraints model_end
  | pred_decl_items            variables constraints model_end
  | pred_decl_items                      constraints model_end

pred_decl_items:
    pred_decl_items pred_decl_item ';'
/*| pred_decl_items error ';' { yyerrok; } */
  | /* empty */

parameters:
    parameters parameter ';'
  | parameter ';'

variables:
    variables variable ';'
  | variable ';'

constraints: constraints constraint ';'
               | /* empty */

model_end      : solve_item ';'

//---------------------------------------------------------------------------
// Predicate declarations
//---------------------------------------------------------------------------

pred_decl_item:
    PREDICATE NEW_IDENT '(' pred_decl_args ')' {
      reader->error(@NEW_IDENT, "Predicate '", $NEW_IDENT, ", ' is not supported.");
      free($2);
      YYABORT;
    }
  | PREDICATE ALL_DIFFERENT_INT '(' ARRAY '[' INT ']' OF VAR INT ':' anyid ')'
  | PREDICATE ARRAY_FLOAT_MAXIMUM'('VAR FLOAT':' anyid ',' ARRAY'['INT']' OF VAR FLOAT':' anyid')'
  | PREDICATE ARRAY_FLOAT_MINIMUM'('VAR FLOAT':' anyid',' ARRAY'['INT']' OF VAR FLOAT':' anyid')'
  | PREDICATE ARRAY_INT_MAXIMUM'('VAR INT':' anyid ',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE ARRAY_INT_MINIMUM'('VAR INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE BIN_PACKING_CAPA'('ARRAY'['INT']' OF INT':' anyid ',' ARRAY'['INT']' OF VAR INT':' anyid ',' ARRAY'['INT']' OF INT':' anyid')'
  | PREDICATE BIN_PACKING_LOAD'('ARRAY'['INT']' OF VAR INT':' anyid ',' ARRAY'['INT']' OF VAR INT':' anyid ',' ARRAY'['INT']' OF INT':' anyid')'
  | PREDICATE BOOL_CLAUSE_REIF'('ARRAY'['INT']' OF VAR BOOL':' anyid',' ARRAY'['INT']' OF VAR BOOL':' anyid',' VAR BOOL':' anyid')'
  | PREDICATE COUNT_EQ_PAR'('ARRAY'['INT']' OF VAR INT':' anyid ',' INT':' anyid ',' VAR INT':' anyid')'
  | PREDICATE CUMULATIVE '(' ARRAY'['INT']' OF VAR INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid',' VAR INT':' anyid')'
  | PREDICATE CPO_INVERSE'('INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid',' INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE INT_POW_FIXED'('VAR INT':' anyid',' INT':' anyid',' VAR INT':' anyid')'
  | PREDICATE LEX_LESSEQ_BOOL'('ARRAY'['INT']' OF VAR BOOL':' anyid',' ARRAY'['INT']' OF VAR BOOL':' anyid')'
  | PREDICATE LEX_LESSEQ_INT'('ARRAY'['INT']' OF VAR INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE LEX_LESS_BOOL'('ARRAY'['INT']' OF VAR BOOL':' anyid',' ARRAY'['INT']' OF VAR BOOL':' anyid')'
  | PREDICATE LEX_LESS_INT'('ARRAY'['INT']' OF VAR INT':' anyid',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE NVALUE'('VAR INT':' anyid ',' ARRAY'['INT']' OF VAR INT':' anyid')'
  | PREDICATE CPO_SUBCIRCUIT '(' INT':' anyid',' ARRAY '[' INT ']' OF VAR INT ':' anyid ')'
  | PREDICATE TABLE_BOOL'('ARRAY'['INT']' OF VAR BOOL':' anyid',' ARRAY'['INT',' INT']' OF BOOL':' anyid')'
  | PREDICATE TABLE_INT'('ARRAY'['INT']' OF VAR INT':' anyid',' ARRAY'['INT',' INT']' OF INT':' anyid')'

anyid:
    NEW_IDENT { free($1); }

//---------------------------------------------------------------------------
// Parameter declarations
//---------------------------------------------------------------------------

parameter:
    FLOAT ':' NEW_IDENT '=' constfloatexpr  { reader->addName($5, $3, @3); free($3); }
  | INT ':' NEW_IDENT '=' constintexpr      { reader->addName($5, $3, @3); free($3); }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF FLOAT ':' NEW_IDENT '=' floatarray {
      if (!reader->verifyArrayRange($3, @3, $5, @5, $floatarray.getSize()))
        YYERROR;
      reader->addName($floatarray, $NEW_IDENT, @10);
      free($NEW_IDENT);
    }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF INT ':' NEW_IDENT '=' intarray {
      if (!reader->verifyArrayRange($3, @3, $5, @5, $intarray.getSize()))
        YYERROR;
      reader->addName($intarray, $NEW_IDENT, @10);
      free($NEW_IDENT);
    }
  | BOOL ':' NEW_IDENT '=' constboolexpr    { reader->addName($5, $3, @3); free($3); }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF BOOL ':' NEW_IDENT '=' boolarray  {
      if (!reader->verifyArrayRange($3, @3, $5, @5, $boolarray.getSize()))
        YYERROR;
      reader->addName($boolarray, $NEW_IDENT, @10);
      free($NEW_IDENT);
    }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF SET OF INT ':' NEW_IDENT '=' '[' intsetlist ']' {
      reader->warning(@NEW_IDENT, "Ignoring set of int '", $NEW_IDENT, "' (not supported).");
      free($NEW_IDENT);
    }

intsetlist:
    intsetlist ',' intset
  | intset

intset:
    '{' intlist '}' { delete $intlist; }
  | INT_LITERAL ".." INT_LITERAL

//---------------------------------------------------------------------------
// Variable declarations
//---------------------------------------------------------------------------

// Note that set domain is missing because set variables should be eliminated
// already during minizinc flattening.
// TODO/2: Float domain is missing, i.e. there should be a rules with FLOAT.
domain:
    INT                            { $$ = new Domain(Domain::Integer); }
  | BOOL                           { $$ = new Domain(Domain::Boolean); }
  | FLOAT                          { $$ = new Domain(Domain::Float); }
  | constint ".." constint         { $$ = new Domain($1, $3); }
  // We cannot use rule constfloat ".." constfloat because it would lead to shift/reduce conflicts.
  // So instead we split it into two rules. This way the parser always knows how to continue just by looking at the literal.
  | constint ".." pureconstfloat   { $$ = new Domain((IlcFloat)$1, $3); }
  | pureconstfloat ".." constfloat { $$ = new Domain($1, $3); }
  | '{' explicitdomain '}'         { $$ = $explicitdomain; }

explicitdomain:
    constint                      {  /* Start with empty domain: */ $$ = new Domain(IlcIntMax, IlcIntMin); $$->add($constint);}
  | explicitdomain ',' constint   { $$ = $1; $$->add($constint); }

  /* TODO/2: float variables are missing. We could allow at least float variables with is_defined_var. */
variable:
    VAR domain ':' NEW_IDENT var_annotations {
      reader->createVar($NEW_IDENT, $domain, @NEW_IDENT, $var_annotations);
      FznReader::deleteVarArgs($domain, $NEW_IDENT, $var_annotations);
    }
  | VAR domain ':' NEW_IDENT var_annotations '=' constint  {
      reader->createVar($NEW_IDENT, $domain, @NEW_IDENT, $var_annotations, $constint, @5);
      FznReader::deleteVarArgs($domain, $NEW_IDENT, $var_annotations);
    }
  | VAR domain ':' NEW_IDENT var_annotations '=' intvar {
      $intvar.name($NEW_IDENT);
      if ($var_annotations->_isOutputVar)
        reader->addOutput($intvar, $NEW_IDENT);
      if ($var_annotations->_isDefinedVar)
        reader->markDefinedVar($intvar, @var_annotations);
      FznReader::deleteVarArgs($domain, $NEW_IDENT, $var_annotations);
    }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF VAR domain ':' NEW_IDENT var_annotations {
      reader->createVarArray($NEW_IDENT, $domain, $3, $5, @NEW_IDENT, $var_annotations);
      FznReader::deleteVarArgs($domain, $NEW_IDENT, $var_annotations);
    }
  | ARRAY '[' INT_LITERAL ".." INT_LITERAL ']' OF VAR domain ':' NEW_IDENT var_annotations '=' intexprarray {
      reader->createVarArray($NEW_IDENT, $domain, $3, $5, @NEW_IDENT, $intexprarray, $var_annotations);
      FznReader::deleteVarArgs($domain, $NEW_IDENT, $var_annotations);
    }


//---------------------------------------------------------------------------
// Constraints
//---------------------------------------------------------------------------

constraint:
    CONSTRAINT constraint_elem

/* Value of constraint annotation is a variable (or nullpr) that the constraint defines */
c_annots:
    c_annots COLONCOLON c_annot {
      if ($1->_definedVar) {
        if ($3->_definedVar)
          reader->warning(@3, "Annotation defines_var is specified more than once for the constraint.");
        $$ = $1;
        delete $3;
      } else {
        $$ = $3;
        delete $1;
      }
    }
  | /* empty */ { $$ = new ConstraintAnnotations(); }

c_annot:
    DEFINES_VAR '(' FLOAT_VAR ')'  { $$ = new ConstraintAnnotations($3, @1); }
  | DEFINES_VAR '(' INT_VAR ')'    { $$ = new ConstraintAnnotations($3, @1); }
  | DEFINES_VAR '(' BOOL_VAR ')'   { $$ = new ConstraintAnnotations($3, @1); }
  | any_annotation                 { $$ = new ConstraintAnnotations(); }

constraint_elem:
    INT_ABS '(' intexpr ',' intexpr ')' c_annots                              { reader->assign($5, MODEL.abs($3), @1, $c_annots); }
  | INT_DIV '(' intexpr ',' intexpr ',' intexpr ')' c_annots                  { reader->assign($7, $3/$5, @1, $c_annots); }
  | INT_EQ '(' intexpr ',' intexpr  ')' c_annots                              { reader->equal($3, $5, @1, $c_annots); }
  | INT_EQ_REIF '(' intexpr ',' intexpr ',' boolexpr ')' c_annots             { reader->assign($7, $3==$5, @1, $c_annots); }
  | INT_LE '(' intexpr ',' intexpr ')' c_annots                               { reader->add($3<=$5, @1, $c_annots); }
  | INT_LE_REIF '(' intexpr ',' intexpr ',' boolexpr ')' c_annots             { reader->assign($7, $3<=$5, @1, $c_annots); }
  | INT_LIN_EQ '(' intarray ',' intexprarray ',' constint ')' c_annots        { reader->assignFromLinEq($3, $5, $7, @1, $c_annots); }
  | INT_LIN_EQ_REIF '(' intarray ',' intexprarray ',' constintexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5) == $7, @1, $c_annots); }
  | INT_LIN_LE '(' intarray ',' intexprarray ',' constintexpr ')' c_annots    { reader->add(MODEL.scalProd($3, $5) <= $7, @1, $c_annots); }
  | INT_LIN_LE_REIF '(' intarray ',' intexprarray ',' constintexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5) <= $7, @1, $c_annots); }
  | INT_LIN_NE '(' intarray ',' intexprarray ',' constintexpr ')' c_annots    { reader->add(MODEL.scalProd($3, $5) != $7, @1, $c_annots); }
  | INT_LIN_NE_REIF '(' intarray ',' intexprarray ',' constintexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5) != $7, @1, $c_annots); }
  | INT_LT '(' intexpr ',' intexpr ')' c_annots                               { reader->add($3<$5, @1, $c_annots); }
  | INT_LT_REIF '(' intexpr ',' intexpr ',' boolexpr ')' c_annots             { reader->assign($7, $3<$5, @1, $c_annots); }
  | INT_CMAX '(' intexpr ',' intexpr ',' intexpr ')' c_annots                 { reader->assign($7, MODEL.max($3, $5), @1, $c_annots); }
  | INT_CMIN '(' intexpr ',' intexpr ',' intexpr ')' c_annots                 { reader->assign($7, MODEL.min($3, $5), @1, $c_annots); }
  | INT_MOD '(' intexpr ',' intexpr ',' intexpr ')' c_annots                  { reader->assign($7, $3 % $5, @1, $c_annots); }
  | INT_NE '(' intexpr ',' intexpr ')' c_annots                               { reader->add($3!=$5, @1, $c_annots); }
  | INT_NE_REIF '(' intexpr ',' intexpr ',' boolexpr ')' c_annots             { reader->assign($7, $3!=$5, @1, $c_annots); }
  | INT_PLUS '(' intexpr ',' intexpr ',' intexpr ')' c_annots                 { reader->plus($3, $5, $7, @1, $c_annots); }
  | INT_POW '(' intexpr ',' intexpr ',' intexpr ')' c_annots                  { reader->assign($7, MODEL.floatToInt(MODEL.exp($3, $5)), @1, $c_annots); }
  | INT_POW_FIXED '(' intexpr ',' constint ',' intexpr ')' c_annots           { reader->assign($7, reader->intPowFixed($3, $5, @1), @1, $c_annots); }
  | INT_TIMES '(' intexpr ',' intexpr ',' intexpr ')' c_annots                { reader->assign($7, $3*$5, @1, $c_annots); }
  | INT_TO_FLOAT '(' intexpr ',' floatexpr ')' c_annots                       { reader->assign($5, $3, @1, $c_annots); }
  | ARRAY_BOOL_AND '(' boolexprarray ',' boolexpr ')' c_annots                { reader->assign($5, MODEL.conjunction($3), @1, $c_annots); }
  | ARRAY_BOOL_ELEMENT '(' intexpr ',' boolarray ',' boolexpr ')' c_annots    {
      /* Flatzinc counts arrays from 1 while CPO is counting from 0 */
      reader->assign($7, 0 != MODEL.element($5, $3-1), @1, $c_annots);
    }
  | ARRAY_BOOL_OR '(' boolexprarray ',' boolexpr ')' c_annots                 { reader->assign($5, MODEL.disjunction($3), @1, $c_annots); }
  | ARRAY_BOOL_XOR '(' boolexprarray ')' c_annots                             { reader->assignFromArrayXor($3, @1, $c_annots); }
  | ARRAY_FLOAT_ELEMENT '(' intexpr ',' floatarray ',' floatexpr ')' c_annots
       /* Flatzinc counts arrays from 1 while CPO is counting from 0 */
      { reader->assign($7, MODEL.element($5, $3-1), @1, $c_annots); }
  | ARRAY_INT_ELEMENT '(' intexpr ',' intarray ',' intexpr ')' c_annots
       /* Flatzinc counts arrays from 1 while CPO is counting from 0 */
      { reader->assign($7, MODEL.element($5, $3-1), @1, $c_annots); }
  | ARRAY_VAR_BOOL_ELEMENT '(' intexpr ',' boolexprarray ',' boolexpr ')' c_annots
       /* Flatzinc counts arrays from 1 while CPO is counting from 0 */
      { reader->assign($7, 0 != MODEL.element($5, $3-1), @1, $c_annots); }
  | ARRAY_VAR_FLOAT_ELEMENT '(' intexpr ',' floatexprarray ',' floatexpr ')' c_annots
      { reader->error(@ARRAY_VAR_FLOAT_ELEMENT, "Predicate not supported: array_var_float_element."); (void)$c_annots; }
  | ARRAY_VAR_INT_ELEMENT '(' intexpr ',' intexprarray ',' intexpr ')' c_annots
       /* Flatzinc counts arrays from 1 while CPO is counting from 0 */
      { reader->assign($7, MODEL.element($5, $3-1), @1, $c_annots); }
  | BOOL2INT '(' boolexpr ',' intexpr ')' c_annots                            { reader->assign($5, $3, @1, $c_annots); }
  | BOOL_AND '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots              { reader->assign($7, $3&&$5, @1, $c_annots); }
  | BOOL_CLAUSE '(' boolexprarray ',' boolexprarray ')' c_annots              { reader->add(reader->clause($3, $5), @1, $c_annots); }
  | BOOL_CLAUSE_REIF'('boolexprarray',' boolexprarray',' boolexpr')' c_annots { reader->assign($7, reader->clause($3, $5), @1, $c_annots); }
  | BOOL_EQ '(' boolexpr ',' boolexpr ')' c_annots                            { reader->equal($3, $5, @1, $c_annots); }
  | BOOL_EQ_REIF '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots          { reader->assign($7, $3==$5, @1, $c_annots); }
  | BOOL_LE '(' boolexpr ',' boolexpr ')' c_annots                            { reader->add($3<=$5, @1, $c_annots); }
  | BOOL_LE_REIF '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots          { reader->assign($7, $3<=$5, @1, $c_annots); }
  | BOOL_LIN_EQ '(' intarray ',' boolexprarray ',' intexpr ')' c_annots       { reader->assign($7, MODEL.scalProd($3, $5), @1, $c_annots); }
  | BOOL_LIN_LE '(' intarray ',' boolexprarray ',' constintexpr ')' c_annots  { reader->add(MODEL.scalProd($3, $5) <= $7, @1, $c_annots); }
  | BOOL_LT '(' boolexpr ',' boolexpr ')' c_annots                            { reader->add($3<$5, @1, $c_annots); }
  | BOOL_LT_REIF '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots          { reader->assign($7, $3<$5, @1, $c_annots); }
  | BOOL_NOT '(' boolexpr ',' boolexpr ')' c_annots                           { reader->assign($5, !$3, @1, $c_annots); }
  | BOOL_OR '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots               { reader->assign($7, $3||$5, @1, $c_annots); }
    /* For some reason there are two versions of XOR: with and without the last argument. And it is not called bool_xor_reif. */
  | BOOL_XOR '(' boolexpr ',' boolexpr ')' c_annots                           { reader->add($3!=$5, @1, $c_annots); }
  | BOOL_XOR '(' boolexpr ',' boolexpr ',' boolexpr ')' c_annots              { reader->assign($7, $3!=$5, @1, $c_annots); }
  | FLOAT_ABS '(' floatexpr ',' floatexpr ')' c_annots                        { reader->assign($5, MODEL.abs($3), @1, $c_annots); }
  | FLOAT_DIV '(' floatexpr ',' floatexpr ',' floatexpr ')' c_annots          { reader->assign($7, $3/$5, @1, $c_annots); }
  | FLOAT_EXP '(' floatexpr ',' floatexpr ')' c_annots                        { reader->assign($5, MODEL.exponent($3), @1, $c_annots); }
  | FLOAT_LN '(' floatexpr ',' floatexpr ')' c_annots                         { reader->assign($5, MODEL.log($3), @1, $c_annots); }
  | FLOAT_LOG10 '(' floatexpr ',' floatexpr ')' c_annots                      { reader->assign($5, MODEL.log($3)/IlcLog(10), @1, $c_annots); }
  | FLOAT_LOG2 '(' floatexpr ',' floatexpr ')' c_annots                       { reader->assign($5, MODEL.log($3)/IlcLog(2), @1, $c_annots); }
  | FLOAT_EQ '(' floatexpr ',' floatexpr ')' c_annots                         { reader->equal($5, $3, @1, $c_annots); }
  | FLOAT_EQ_REIF '(' floatexpr ',' floatexpr ',' boolexpr ')' c_annots       { reader->assign($7, $3==$5, @1, $c_annots); }
  | FLOAT_IN '(' floatexpr',' constfloat',' constfloat')' c_annots            { reader->add(MODEL.range($3, $5, $7), @1, $c_annots); }
  | FLOAT_IN_REIF'('floatexpr','constfloat','constfloat','boolexpr')' c_annots{ reader->assign($9, MODEL.range($3, $5, $7), @1, $c_annots); }
  | FLOAT_LE '(' floatexpr ',' floatexpr ')' c_annots                         { reader->add($3<=$5, @1, $c_annots); }
  | FLOAT_LE_REIF '(' floatexpr ',' floatexpr ',' boolexpr ')' c_annots       { reader->assign($7, $3<=$5, @1, $c_annots); }
  | FLOAT_LIN_EQ '(' floatarray ',' floatexprarray ',' constfloat ')' c_annots{ reader->assignFromLinEq($3, $5, $7, @1, $c_annots); }
  | FLOAT_LIN_EQ_REIF '(' floatarray ',' floatexprarray ',' constfloatexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5)==$7, @1, $c_annots); }
  | FLOAT_LIN_LE '(' floatarray ',' floatexprarray ',' constfloatexpr ')' c_annots
      { reader->add(MODEL.scalProd($3, $5)<=$7, @1, $c_annots); }
  | FLOAT_LIN_LE_REIF '(' floatarray ',' floatexprarray ',' constfloatexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5)<=$7, @1, $c_annots); }
  | FLOAT_LIN_LT '(' floatarray ',' floatexprarray ',' constfloatexpr ')' c_annots
      { reader->add(MODEL.scalProd($3, $5)<$7, @1, $c_annots); }
  | FLOAT_LIN_LT_REIF '(' floatarray ',' floatexprarray ',' constfloatexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5)<$7, @1, $c_annots); }
  | FLOAT_LIN_NE '(' floatarray ',' floatexprarray ',' constfloatexpr ')' c_annots
      { reader->add(MODEL.scalProd($3, $5)!=$7, @1, $c_annots); }
  | FLOAT_LIN_NE_REIF '(' floatarray ',' floatexprarray ',' constfloatexpr ',' boolexpr ')' c_annots
      { reader->assign($9, MODEL.scalProd($3, $5)!=$7, @1, $c_annots); }
  | FLOAT_LT '(' floatexpr ',' floatexpr ')' c_annots                         { reader->add($3<$5, @1, $c_annots); }
  | FLOAT_LT_REIF '(' floatexpr ',' floatexpr ',' boolexpr ')' c_annots       { reader->assign($7, $3<$5, @1, $c_annots); }
  | FLOAT_MAX '(' floatexpr ',' floatexpr ',' floatexpr ')' c_annots          { reader->assign($7, MODEL.max($3, $5), @1, $c_annots); }
  | FLOAT_MIN '(' floatexpr ',' floatexpr ',' floatexpr ')' c_annots          { reader->assign($7, MODEL.min($3, $5), @1, $c_annots); }
  | FLOAT_NE '(' floatexpr ',' floatexpr ')' c_annots                         { reader->add($3!=$5, @1, $c_annots); }
  | FLOAT_NE_REIF '(' floatexpr ',' floatexpr ',' boolexpr ')' c_annots       { reader->assign($7, $3!=$5, @1, $c_annots); }
  | FLOAT_POW '(' floatexpr ',' floatexpr ',' floatexpr ')' c_annots          { reader->assign($7, MODEL.exp($3, $5), @1, $c_annots); }
  | FLOAT_PLUS '(' floatexpr ',' floatexpr ',' floatexpr ')' c_annots         { reader->plus($3, $5, $7, @1, $c_annots); }
  | FLOAT_SQRT '(' floatexpr ',' floatexpr ')' c_annots                       { reader->assign($5, MODEL.exp($3, 0.5), @1, $c_annots); }
  /* SET_IN and SET_IN_REIF can specify the set either by enumeration or as a range.
     If it is a range then we can save memory and use range instead of allowedAssignment. */
  | SET_IN '(' intexpr ',' constint ".." constint ')' c_annots                { reader->add(MODEL.range($3, $5, $7), @1, $c_annots); }
  | SET_IN '(' intexpr ',' '{' intlist '}' ')' c_annots                       { reader->add(MODEL.allowedAssignments($3, MODEL.intArray(*$intlist)), @1, $c_annots); delete $intlist; }
  | SET_IN_REIF'(' intexpr ',' constint".."constint ',' boolexpr ')' c_annots { reader->assign($9, MODEL.range($3, $5, $7), @1, $c_annots); }
  | SET_IN_REIF'(' intexpr ',' '{' intlist '}' ',' boolexpr ')' c_annots      { reader->assign($boolexpr, MODEL.allowedAssignments($3, MODEL.intArray(*$intlist)), @1, $c_annots); delete $intlist; }
  | ALL_DIFFERENT_INT '(' intexprarray ')' c_annots                           { reader->add(MODEL.allDiff($3), @1, $c_annots); }
  | CPO_SUBCIRCUIT '(' constint ',' intexprarray ')' c_annots                     {
      // In CPO, arrays are indexed from 0, however in minizinc from any value (the first argument).
      // So we need to shift all the variables.
      reader->add(MODEL.subCircuit(reader->incrArrayItems($5, -$3)), @1, $c_annots);
  }
  | COUNT_EQ_PAR '(' intexprarray ',' constint ',' intexpr ')' c_annots       { reader->assign($7, MODEL.count($3, $5), @1, $c_annots); }
  | CUMULATIVE '(' intexprarray ',' intexprarray ',' intexprarray ',' intexpr ')' c_annots {
      reader->add(reader->cumulative($3, $5, $7, $9, @1), @1, $c_annots);
    }
  | CPO_INVERSE '(' constint ',' intexprarray ',' constint ',' intexprarray ')' c_annots {
      // Inverse in CP Optimizer always indexes from 0. However in here f/$5 starts by fShift/$3 and inv/$9 by invShift/$7.
      // So we "move" the arrays by appropriate shifts.
      reader->add(MODEL.inverse(reader->incrArrayItems($5, -$3), reader->incrArrayItems($9, -$7)), @1, $c_annots);
    }
  | LEX_LESSEQ_BOOL '(' boolexprarray ',' boolexprarray ')' c_annots          { reader->add(MODEL.lexicographic($3, $5), @1, $c_annots); }
  | LEX_LESSEQ_INT '(' intexprarray ',' intexprarray ')' c_annots             { reader->add(MODEL.lexicographic($3, $5), @1, $c_annots); }
  | LEX_LESS_BOOL '(' boolexprarray ',' boolexprarray ')' c_annots            { reader->add(MODEL.strictLexicographic($3, $5), @1, $c_annots); }
  | LEX_LESS_INT '(' intexprarray ',' intexprarray ')' c_annots               { reader->add(MODEL.strictLexicographic($3, $5), @1, $c_annots); }
  | ARRAY_FLOAT_MAXIMUM '(' floatexpr ',' floatexprarray ')' c_annots         { reader->assign($3, MODEL.max($5), @1, $c_annots); }
  | ARRAY_INT_MAXIMUM '(' intexpr ',' intexprarray ')' c_annots               { reader->assign($3, MODEL.max($5), @1, $c_annots); }
  | ARRAY_FLOAT_MINIMUM '(' floatexpr ',' floatexprarray ')' c_annots         { reader->assign($3, MODEL.min($5), @1, $c_annots); }
  | ARRAY_INT_MINIMUM '(' intexpr ',' intexprarray ')' c_annots               { reader->assign($3, MODEL.min($5), @1, $c_annots); }
  | TABLE_BOOL'(' boolexprarray ',' boolarray ')' c_annots                    { reader->add(reader->tableConstraint($3, $5, @1), @1, $c_annots); }
  | TABLE_BOOL_REIF'('boolexprarray',' boolarray',' boolexpr')' c_annots      { reader->assign($7, reader->tableConstraint($3, $5, @1), @1, $c_annots); }
  | TABLE_INT '(' intexprarray ',' intarray ')' c_annots                      { reader->add(reader->tableConstraint($3, $5, @1), @1, $c_annots); }
  | TABLE_INT_REIF'('intexprarray',' intarray',' boolexpr')' c_annots         { reader->assign($7, reader->tableConstraint($3, $5, @1), @1, $c_annots); }
  | BIN_PACKING_LOAD'('intexprarray',' intexprarray',' intarray')' c_annots   { reader->add(MODEL.pack(reader->addPrefix(0, $3), $5, $7), @1, $c_annots); }
  | BIN_PACKING_CAPA '(' intarray ',' intexprarray ',' intarray ')' c_annots  { reader->add(reader->packFixedCaps($3, $5, $7), @1, $c_annots); }
  | NVALUE '(' intexpr ',' intexprarray ')' c_annots                          { reader->assign($3, MODEL.countDifferent($5), @1, $c_annots); }

//---------------------------------------------------------------------------
// Common stuff
//---------------------------------------------------------------------------

// It seems that array indexing rules such as:
//    INT_VAR_ARRAY '[' constint ']'
// are no longer necessary (since some version of flatzinc).
// It is not a big deal to keep them though so we can read old files too.

//---------------------------------------------------------------------------
// Constants, parsed into IlcInt or IlcFloat
//---------------------------------------------------------------------------

constbool:
    BOOL_LITERAL                { $$ = $1; }
  | BOOL_CONSTANT               { $$ = $1.getValue(); }
  | BOOL_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

constint:
    constbool                   { $$ = $1; }
  | INT_LITERAL                 { $$ = $1; }
  | INT_CONSTANT                { $$ = $1.getValue(); }
  | INT_ARRAY '[' constint ']'  {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

pureconstfloat:
    FLOAT_LITERAL                { $$ = $1; }
  | FLOAT_CONSTANT               { $$ = $1.getValue(); }
  | FLOAT_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

constfloat:
    constint                     { $$ = (IlcFloat)$1; }
  | pureconstfloat               { $$ = $1; }

//---------------------------------------------------------------------------
// The same constants, but returned in BoolExpr, IntExpr or FloatExpr
//---------------------------------------------------------------------------
// Those rules can be used in rules that accept variable or a constant.
// We could leverage rules above and just create e.g. intExpr from IlcInt
// (because from the parsing point of view constint and constintexpr are
// identical). However it would be slightly more time consuming: in some cases
// we find a constant by name in the engine and then we would create another
// constant for it.

constboolexpr:
    BOOL_LITERAL                { $$ = MODEL.boolExpr($1); }
  | BOOL_CONSTANT               { $$ = $1; }
  | BOOL_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = MODEL.boolExpr($1[$3 - 1]);
    }

constintexpr:
    constboolexpr               { $$ = $1; }
  | INT_CONSTANT                { $$ = $1; }
  | INT_LITERAL                 { $$ = MODEL.intExpr($1); }
  | INT_ARRAY '[' constint ']'  {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = MODEL.intExpr($1[$3 - 1]);
    }

constfloatexpr:
    constintexpr                 { $$ = $1; }
  | FLOAT_LITERAL                { $$ = MODEL.floatExpr($1); }
  | FLOAT_CONSTANT               { $$ = $1; }
  | FLOAT_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = MODEL.floatExpr($1[$3 - 1]);
    }

//---------------------------------------------------------------------------
// "Real" expressions, i.e. non-constant BoolExpr, IntExpr and FloatExpr.
//---------------------------------------------------------------------------
// They are not required directly in flatzinc rules, they factorize common code
// for rules intexpr, floatexpr etc.

boolvar:
    BOOL_VAR                        { $$ = $1; }
  | BOOL_VAR_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

intvar:
    boolvar                        { $$ = $1; }
  | INT_VAR                        { $$ = $1; }
  | INT_VAR_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

floatvar:
    intvar                           { $$ = $1; }
  | FLOAT_VAR                        { $$ = $1; }
  | FLOAT_VAR_ARRAY '[' constint ']' {
      if (!reader->verifyArrayIndex($3, @3, $1.getSize()))
        YYERROR;
      $$ = $1[$3 - 1];
    }

//---------------------------------------------------------------------------
// Scalar flatzinc constraint arguments
//---------------------------------------------------------------------------
// Flatzinc allows to use a constant whenever a variable is required.

  /* Valid argument for a constraint requiring int var. */
intexpr:
    intvar        { $$ = $1; }
  | constintexpr  { $$ = $1; }

  /* Valid argument for a constraint requiring float var. */
floatexpr:
    floatvar       { $$ = $1; }
  | constfloatexpr { $$ = $1; }

  /* Valid argument for a constraint requiring bool var. */
boolexpr:
    boolvar        { $$ = $1; }
  | constboolexpr  { $$ = $1; }

//---------------------------------------------------------------------------
// BoolArray
//---------------------------------------------------------------------------

boolarray:
    '[' boollist ']'     { $$ = MODEL.intArray(*$2); delete $2; }
  | BOOL_ARRAY           { $$ = $1; }

boollist:
    boollistnotempty { $$ = $1; }
  | /* empty */      { $$ = new IntVector; }

boollistnotempty:
    constbool                 { $$ = new IntVector; $$->push_back($1);}
  | boollist ',' constbool    { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// IntArray
//---------------------------------------------------------------------------

intarray:
    INT_ARRAY        { $$ = $1; }
  | BOOL_ARRAY       { $$ = $1; }
  | '[' intlist ']'  { $$ = MODEL.intArray(*$intlist); delete $intlist; }

intlist:
    intlistnotempty  { $$ = $1; }
  | /* empty */      { $$ = new IntVector; }

intlistnotempty:
    constint              { $$ = new IntVector; $$->push_back($1);}
  | intlist ',' constint  { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// FloatArray
//---------------------------------------------------------------------------

floatarray:
    FLOAT_ARRAY        { $$ = $1; }
  | INT_ARRAY          { $$ = MODEL.floatArray($1); }
  | BOOL_ARRAY         { $$ = MODEL.floatArray($1); }
  | '[' floatlist ']'  { $$ = MODEL.floatArray(*$2); delete $2; }

floatlist:
    floatlistnotempty { $$ = $1; }
  | /* empty */       { $$ = new FloatVector; }

floatlistnotempty:
    constfloat               { $$ = new FloatVector; $$->push_back($1);}
  | floatlist ',' constfloat { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// BoolExprArray
//---------------------------------------------------------------------------

boolexprarray:
    '[' boolexprlist ']'  { $$ = MODEL.boolExprArray(*$2); delete $2; }
  | BOOL_VAR_ARRAY        { $$ = $1; }
  | BOOL_ARRAY            {
      // CP Optimizer doesn't have BoolArray so we are using IntArray instead.
      // So the conversion to BoolExprArray must be manual:
      BoolExprVector tmp;
      tmp.reserve($1.getSize());
      for (IlcInt v: $1) {
        assert(v >= 0 && v <= 1);
        tmp.push_back(MODEL.boolExpr(v != 0));
      }
      $$ = MODEL.boolExprArray(tmp);
    }

boolexprlist:
    boolexprlistnotempty { $$ = $1; }
  | /* empty */          { $$ = new BoolExprVector; }

boolexprlistnotempty:
    boolexpr                  { $$ = new BoolExprVector; $$->push_back($1);}
  | boolexprlist ',' boolexpr { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// IntExprArray
//---------------------------------------------------------------------------
// Flatzinc allows to use also IntArray of BoolExprArray whenever array of
// integer variables argument is required.

intexprarray:
    INT_VAR_ARRAY        { $$ = $1; }
  | BOOL_VAR_ARRAY       { $$ = $1; }
  | INT_ARRAY            { $$ = MODEL.intExprArray($1); }
  | BOOL_ARRAY           { $$ = MODEL.intExprArray($1); }
  | '[' intexprlist ']'  { $$ = MODEL.intExprArray(*$2); delete $2; }

intexprlist:
    intexprlistnotempty { $$ = $1; }
  | /* empty */         { $$ = new IntExprVector; }

intexprlistnotempty:
    intexpr                 { $$ = new IntExprVector; $$->push_back($1);}
  | intexprlist ',' intexpr { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// FloatExprArray
//---------------------------------------------------------------------------
// Flatzinc allows to use also e.g. IntArray of IntExprArray whenever array of
// float variables argument is required.

floatexprarray:
    FLOAT_VAR_ARRAY        { $$ = $1; }
  | INT_VAR_ARRAY          { $$ = $1; }
  | BOOL_VAR_ARRAY         { $$ = $1; }
  | '[' floatexprlist ']'  { $$ = MODEL.floatExprArray(*$2); delete $2; }
  | FLOAT_ARRAY            { $$ = MODEL.floatExprArray($1); }
  | INT_ARRAY              { $$ = MODEL.floatExprArray($1); }
  | BOOL_ARRAY             { $$ = MODEL.floatExprArray($1); }

floatexprlist:
    floatexprlistnotempty  { $$ = $1; }
  | /* empty */            { $$ = new FloatExprVector; }

floatexprlistnotempty:
    floatexpr                   { $$ = new FloatExprVector; $$->push_back($1);}
  | floatexprlist ',' floatexpr { $$ = $1; $$->push_back($3); }

//---------------------------------------------------------------------------
// Solve
//---------------------------------------------------------------------------

solve_item:
    SOLVE solve_annotations solve_kind

solve_kind:
    SATISFY
  | MINIMIZE floatexpr  { reader->addObjective(MODEL.minimize($2), @2); }
  | MAXIMIZE floatexpr  { reader->addObjective(MODEL.maximize($2), @2); }

//---------------------------------------------------------------------------
// Predicate parameters
//---------------------------------------------------------------------------

pred_decl_args:
    pred_decl_args ',' pred_decl_arg
  | pred_decl_arg

pred_decl_arg:
     non_array_ti_expr_tail ':' anyid
   | VAR non_array_ti_expr_tail ':' anyid
   | ARRAY '[' pred_arg_array_index ']' OF  pred_arg_array_tail ':' anyid

pred_arg_array_index:
    INT
  | INT_LITERAL DOTDOT INT_LITERAL

pred_arg_array_tail:
    non_array_ti_expr_tail
  | VAR non_array_ti_expr_tail

//---------------------------------------------------------------------------
// Type-Inst Expression Tails
//---------------------------------------------------------------------------

non_array_ti_expr_tail:
    scalar_ti_expr_tail
  | set_ti_expr_tail

scalar_ti_expr_tail:
    bool_ti_expr_tail
  | int_ti_expr_tail
  | float_ti_expr_tail

bool_ti_expr_tail:
    BOOL

int_ti_expr_tail:
    INT
  | INT_LITERAL DOTDOT INT_LITERAL
  | '{' int_literals '}'

int_literals:
    INT_LITERAL ',' int_literals
  | INT_LITERAL

float_ti_expr_tail:
    FLOAT
  | FLOAT_LITERAL DOTDOT FLOAT_LITERAL

set_ti_expr_tail:
    SET OF int_ti_expr_tail

//---------------------------------------------------------------------------
// Annotations
//---------------------------------------------------------------------------

var_annotations:
     /* empty */ {
       $$ = new VarAnnotations();
    }
  | var_annotations "::" IS_DEFINED_VAR {
      if ($1->_isDefinedVar)
        reader->warning(@IS_DEFINED_VAR, "Annotation is_defined_var is specified twice for the same variable.");
      $$ = $1;
      $$->_isDefinedVar = true;
      $$->_definedLocation = @1;
    }
  | var_annotations "::" VAR_IS_INTRODUCED {
      $$ = $1;
    }
  | var_annotations "::" OUTPUT_ARRAY '(' '[' outputranges ']' ')' {
      if ($1->_isOutputVar)
        reader->error(@OUTPUT_ARRAY, "Annotation is_output_var is specified twice for the same variable.");
      $$ = $1;
      $$->_isOutputVar = true;
      $$->_outputRanges = IntRangeVectorPtr($outputranges);
    }
  | var_annotations "::" OUTPUT_VAR {
      if ($1->_isOutputVar)
        reader->error(@OUTPUT_VAR, "Annotation is_output_var is specified twice for the same variable.");
      $$ = $1;
      $$->_isOutputVar = true;
      assert($$->_outputRanges == nullptr);
    }
  | var_annotations "::" any_annotation {
      /* All remaining annotations are ignored. */
      $$ = $1;
    }

outputranges:
    outputrange                    { $$ = new IntRangeVector; $$->push_back($1); }
  | outputranges ',' outputrange   { $$ = $1; $$->push_back($3); }

outputrange:
    INT_LITERAL ".." INT_LITERAL   { $$._min = $1; $$._max = $3; }

solve_annotations:
    solve_annotations COLONCOLON solve_goal
  | /* empty */

solve_goals:
    solve_goals ',' solve_goal
  | solve_goal

solve_goal:
    search_type '(' floatexprarray ',' varchoice ',' valchoice ',' strategy ')'   { reader->warning(@1, "Ignoring solve goal (not supported)."); }
  | SEQ_SEARCH '(' '[' solve_goals ']' ')'                                        { reader->warning(@1, "Ignoring solve goal (not supported)."); }

search_type:
    INT_SEARCH
  | BOOL_SEARCH

varchoice:
    FIRST_FAIL
  | INPUT_ORDER
  | ANTI_FIRST_FAIL
  | SMALLEST
  | LARGEST
  | OCCURRENCE
  | MOST_CONSTRAINED
  | MAX_REGRET

valchoice:
    INDOMAIN_MIN
  | INDOMAIN_MAX
  | INDOMAIN_MIDDLE
  | INDOMAIN_MEDIAN
  | INDOMAIN
  | INDOMAIN_RANDOM
  | INDOMAIN_SPLIT
  | INDOMAIN_REVERSE_SPLIT
  | INDOMAIN_INTERVAL

strategy:
    COMPLETE

any_annotation:
    NEW_IDENT                      { reader->warning(@NEW_IDENT, "Ignoring annotation '", $1, "' (not supported)."); free($1); }
  | NEW_IDENT '(' any_values ')'   { reader->warning(@NEW_IDENT, "Ignoring annotation '", $1, "' (not supported)."); free($1); }

any_values:
    any_values ',' any_value
  | any_value

any_value:
    floatexpr
  | FLOAT_VAR_ARRAY
  | INT_VAR_ARRAY
  | NEW_IDENT               { free($1); }
  | '[' any_values ']'

%%

static void yyerror(YYLTYPE* location, void* /*scanner*/, FznReader* reader, const char* msg) {
  reader->error(*location, "Parsing error: ", msg);
}
