/*
// --------------------------------------------------------------------------
// Source file provided under Apache License, Version 2.0, January 2004,
// http://www.apache.org/licenses/
// (c) Copyright IBM Corp. 2019, 2020
//
// This code is partly based on earlier contributions from Nick Nethercote
// and Julien Fischer. These contributions are in the public domain.
// --------------------------------------------------------------------------
*/

/* -------------- options section -------------- */

 // Do not include unistd.h:
%option nounistd
 // Assume that yywrap function always return 1 (i.e. we don't have to specify it):
%option noyywrap
 // Reentrant scanner can run in multiple threads:
%option reentrant
 // Type of yyextra variable:
%option extra-type="FznReader*"
 // Changes default "yy" prefix for the following symbols:
 //   yy_create_buffer, yy_delete_buffer, yy_flex_debug , yy_init_buffer ,
 //   yy_flush_buffer, yy_load_buffer_state, yy_switch_to_buffer, yyin, yyleng,
 //   yylex, yylineno, yyout, yyrestart, yytext, yywrap, yyalloc, yyrealloc,
 //   yyfree
%option prefix="FznParse_"
 // Generate scanner for bison:
%option bison-bridge
%option bison-locations
 // The input is file (not user typing on stdin):
%option batch
%option never-interactive
 // Do not assume only 7bit input:
%option 8bit
 // We don't use REJECT feature (it is very slow):
%option noreject
 // Write warning messages:
%option warn
 // Don't generate unput function
%option nounput
 // Faster scanning but bigger table:
%option ecs
%option full
 // Disable bypassing stdio (doesn't compile in windows)
%option noread

%top{
#include "fznreader.h"
#include "fznparser.h"

ILCGCCHIDINGON

#undef YY_INPUT
#define YY_INPUT(buf,result,max) result = yyextra->readInput(buf,max);

// The following action is called for each token just before action associated
// with this token is called. Flex does not compute column location so we use
// it this macro to compute it. Note that the code bellow assumes that yytext
// does not match eoln (and therefore yylloc->_line is no changed). If it
// matches eoln then action associated with this token must deal with it.
#define YY_USER_ACTION                                       \
  {                                                          \
    yylloc->_firstColumn = yylloc->_lastColumn+1;            \
    yylloc->_lastColumn = yylloc->_firstColumn + yyleng - 1; \
  }

// Updates location when new line is seen
#define COUNT_LINE                                   \
    yylloc->_line++;                                 \
    yylloc->_firstColumn = yylloc->_lastColumn = 0;

}

    /* Regular expressions for attributed tokens. */
ident                   [A-Za-z][A-Za-z0-9_]*
underscore_ident        _*[A-Za-z][A-Za-z0-9_]*
    /* String literal cannot span multiple lines. It can contain escaped characters such as \" or \b. */
string_literal          \"([^\\\"\n]|\\.)*\"
int_literal             -?[0-9]+|-?0x[0-9A-Fa-f]+|-?0o[0-7]+
float_literal           -?[0-9]+\.[0-9]+|-?[0-9]+\.[0-9]+[Ee][-+]?[0-9]+|-?[0-9]+[Ee][-+]?[0-9]+

%%

    /* Reserved words */
"array"         { return ARRAY;     }
"bool"          { return BOOL;      }
"constraint"    { return CONSTRAINT;}
"false"         { yylval->_int = 0; return BOOL_LITERAL; }
"float"         { return FLOAT;     }
"int"           { return INT;       }
"minimize"      { return MINIMIZE;  }
"maximize"      { return MAXIMIZE;  }
"of"            { return OF;        }
"predicate"     { return PREDICATE; }
"satisfy"       { return SATISFY;   }
"set"           { return SET;       }
"solve"         { return SOLVE;     }
"true"          { yylval->_int = 1; return BOOL_LITERAL; }
"var"           { return VAR;       }

\.\.            { return DOTDOT;    }
::              { return COLONCOLON;}

    /* Flatzinc standard constraints */
"array_bool_and"             { return ARRAY_BOOL_AND; }
"array_bool_element"         { return ARRAY_BOOL_ELEMENT; }
"array_bool_or"              { return ARRAY_BOOL_OR; }
"array_bool_xor"             { return ARRAY_BOOL_XOR; }
"array_float_element"        { return ARRAY_FLOAT_ELEMENT; }
"array_int_element"          { return ARRAY_INT_ELEMENT; }
"array_var_bool_element"     { return ARRAY_VAR_BOOL_ELEMENT; }
"array_var_float_element"    { return ARRAY_VAR_FLOAT_ELEMENT; }
"array_var_int_element"      { return ARRAY_VAR_INT_ELEMENT; }
"bool2int"                   { return BOOL2INT; }
"bool_and"                   { return BOOL_AND; }
"bool_clause"                { return BOOL_CLAUSE; }
"bool_clause_reif"           { return BOOL_CLAUSE_REIF; }
"bool_eq"                    { return BOOL_EQ; }
"bool_eq_reif"               { return BOOL_EQ_REIF; }
"bool_le"                    { return BOOL_LE; }
"bool_le_reif"               { return BOOL_LE_REIF; }
"bool_lin_eq"                { return BOOL_LIN_EQ; }
"bool_lin_le"                { return BOOL_LIN_LE; }
"bool_lt"                    { return BOOL_LT; }
"bool_lt_reif"               { return BOOL_LT_REIF; }
"bool_not"                   { return BOOL_NOT; }
"bool_or"                    { return BOOL_OR; }
"bool_xor"                   { return BOOL_XOR; }
"float_abs"                  { return FLOAT_ABS; }
"float_div"                  { return FLOAT_DIV; }
"float_exp"                  { return FLOAT_EXP; }
"float_in"                   { return FLOAT_IN; }
"float_in_reif"              { return FLOAT_IN_REIF; }
"float_ln"                   { return FLOAT_LN; }
"float_log10"                { return FLOAT_LOG10; }
"float_log2"                 { return FLOAT_LOG2; }
"float_eq"                   { return FLOAT_EQ; }
"float_eq_reif"              { return FLOAT_EQ_REIF; }
"float_le"                   { return FLOAT_LE; }
"float_le_reif"              { return FLOAT_LE_REIF; }
"float_lin_eq"               { return FLOAT_LIN_EQ; }
"float_lin_eq_reif"          { return FLOAT_LIN_EQ_REIF; }
"float_lin_le"               { return FLOAT_LIN_LE; }
"float_lin_le_reif"          { return FLOAT_LIN_LE_REIF; }
"float_lin_lt"               { return FLOAT_LIN_LT; }
"float_lin_lt_reif"          { return FLOAT_LIN_LT_REIF; }
"float_lin_ne"               { return FLOAT_LIN_NE; }
"float_lin_ne_reif"          { return FLOAT_LIN_NE_REIF; }
"float_lt"                   { return FLOAT_LT; }
"float_lt_reif"              { return FLOAT_LT_REIF; }
"float_max"                  { return FLOAT_MAX; }
"float_min"                  { return FLOAT_MIN; }
"float_ne"                   { return FLOAT_NE; }
"float_ne_reif"              { return FLOAT_NE_REIF; }
"float_plus"                 { return FLOAT_PLUS; }
"float_pow"                  { return FLOAT_POW; }
"float_sqrt"                 { return FLOAT_SQRT; }
"int_eq"                     { return INT_EQ; }
"int_le"                     { return INT_LE; }
"int_abs"                    { return INT_ABS; }
"int_div"                    { return INT_DIV; }
"int_eq_reif"                { return INT_EQ_REIF; }
"int_le_reif"                { return INT_LE_REIF; }
"int_lt"                     { return INT_LT; }
"int_lt_reif"                { return INT_LT_REIF; }
"int_max"                    { return INT_CMAX; }
"int_min"                    { return INT_CMIN; }
"int_mod"                    { return INT_MOD; }
"int_ne"                     { return INT_NE; }
"int_ne_reif"                { return INT_NE_REIF; }
"int_plus"                   { return INT_PLUS; }
"int_times"                  { return INT_TIMES; }
"int2float"                  { return INT_TO_FLOAT; }
"int_lin_eq"                 { return INT_LIN_EQ; }
"int_lin_eq_reif"            { return INT_LIN_EQ_REIF; }
"int_lin_le"                 { return INT_LIN_LE; }
"int_lin_le_reif"            { return INT_LIN_LE_REIF; }
"int_lin_ne"                 { return INT_LIN_NE; }
"int_lin_ne_reif"            { return INT_LIN_NE_REIF; }
"int_pow"                    { return INT_POW; }
"int_pow_fixed"              { return INT_POW_FIXED; }
"set_in"                     { return SET_IN; }
"set_in_reif"                { return SET_IN_REIF; }

    /* Supported global constraints: */
"fzn_all_different_int"   { return ALL_DIFFERENT_INT; }
"cpo_subcircuit"          { return CPO_SUBCIRCUIT; }
"fzn_count_eq_par"        { return COUNT_EQ_PAR; }
"fzn_cumulative"          { return CUMULATIVE; }
"cpo_inverse"             { return CPO_INVERSE; }
"fzn_lex_less_bool"       { return LEX_LESS_BOOL; }
"fzn_lex_lesseq_bool"     { return LEX_LESSEQ_BOOL; }
"fzn_lex_less_int"        { return LEX_LESS_INT; }
"fzn_lex_lesseq_int"      { return LEX_LESSEQ_INT; }
"array_float_maximum"     { return ARRAY_FLOAT_MAXIMUM; }
"array_int_maximum"       { return ARRAY_INT_MAXIMUM; }
"array_float_minimum"     { return ARRAY_FLOAT_MINIMUM; }
"array_int_minimum"       { return ARRAY_INT_MINIMUM; }
"fzn_table_bool"          { return TABLE_BOOL; }
"fzn_table_bool_reif"     { return TABLE_BOOL_REIF; }
"fzn_table_int"           { return TABLE_INT; }
"fzn_table_int_reif"      { return TABLE_INT_REIF; }
"cpo_table_bool"          { return TABLE_BOOL1; }
"cpo_table_bool_reif"     { return TABLE_BOOL_REIF1; }
"cpo_table_int"           { return TABLE_INT1; }
"cpo_table_int_reif"      { return TABLE_INT_REIF1; }
"fzn_bin_packing_load"    { return BIN_PACKING_LOAD; }
"fzn_bin_packing_capa"    { return BIN_PACKING_CAPA; }
"fzn_nvalue"              { return NVALUE; }

    /* Backward compatibility for predicates that were renamed (so we can still
     * read ancient fzn files). We simply map old name to the existing token
     * type as if there is new name in the file. */
"all_different_int"       { return ALL_DIFFERENT_INT; }
"alldifferent_int"        { return ALL_DIFFERENT_INT; }
"bin_packing_capa"        { return BIN_PACKING_CAPA; }
"bin_packing_load"        { return BIN_PACKING_LOAD; }
"count_eq_const"          { return COUNT_EQ_PAR; }
"cumulative"              { return CUMULATIVE; }
"lex_less_bool"           { return LEX_LESS_BOOL; }
"lex_lesseq_bool"         { return LEX_LESSEQ_BOOL; }
"lex_less_int"            { return LEX_LESS_INT; }
"lex_lesseq_int"          { return LEX_LESSEQ_INT; }
"maximum_int"             { return ARRAY_INT_MAXIMUM; }
"maximum_float"           { return ARRAY_FLOAT_MAXIMUM; }
"minimum_float"           { return ARRAY_FLOAT_MINIMUM; }
"minimum_int"             { return ARRAY_INT_MINIMUM; }
"nvalue"                  { return NVALUE; }
"table_bool"              { return TABLE_BOOL; }
"table_int"               { return TABLE_INT; }

    /* Annotations */
"is_defined_var"          { return IS_DEFINED_VAR; }
"var_is_introduced"       { return VAR_IS_INTRODUCED; }
"output_array"            { return OUTPUT_ARRAY; }
"output_var"              { return OUTPUT_VAR; }
"defines_var"             { return DEFINES_VAR; }
"seq_search"              { return SEQ_SEARCH; }
"int_search"              { return INT_SEARCH; }
"bool_search"             { return BOOL_SEARCH; }
"warm_start"              { return WARM_START_SEARCH; }
"constraint_name"         { return CONSTRAINT_NAME_IN_SEARCH; }
"first_fail"              { return FIRST_FAIL; }
"complete"                { return COMPLETE; }
"input_order"             { return INPUT_ORDER; }
"anti_first_fail"         { return ANTI_FIRST_FAIL; }
"smallest"                { return SMALLEST; }
"largest"                 { return LARGEST; }
"occurrence"              { return OCCURRENCE; }
"most_constrained"        { return MOST_CONSTRAINED; }
"max_regret"              { return MAX_REGRET; }
"indomain_min"            { return INDOMAIN_MIN; }
"indomain_max"            { return INDOMAIN_MAX; }
"indomain_middle"         { return INDOMAIN_MIDDLE; }
"indomain_median"         { return INDOMAIN_MEDIAN; }
"indomain"                { return INDOMAIN; }
"indomain_random"         { return INDOMAIN_RANDOM; }
"indomain_split"          { return INDOMAIN_SPLIT; }
"indomain_reverse_split"  { return INDOMAIN_REVERSE_SPLIT; }
"indomain_interval"       { return INDOMAIN_INTERVAL; }

    /* Attributed tokens */
{ident} { return yyextra->decodeSymbol(yytext, yylval); }

{underscore_ident} { return yyextra->decodeSymbol(yytext, yylval); }

{string_literal} {
        // From the doc: Strings have the same syntax as in C programs (namely,
        // they are delimited by double quotes and the backslash character is
        // used for escape sequences. Example: "Hello,\nWorld\t\"quoted!\""
        // Currently this decoding is not done
        yylval->_string = strdup(yytext);
        return STRING_LITERAL;
    }

{int_literal} {
        // Flatzinc uses non-standard 0o prefix for octal numbers so we cannot
        // use strtol directly but verify the prefix.
        // Start by taking away - sign at the beginning (if there is one):
        const char* digits;
        bool negative;
        if (yytext[0] == '-') {
          negative = true;
          digits = yytext+1;
        } else {
          negative = false;
          digits = yytext;
        }
        // There is always at least one digit. Therefore the length of the
        // string is always at least one even after we took '-' away.
        assert(strlen(digits) >= 1);
        // Therefore we can test digits[0] and digits[1], in the worst case
        // digits[1] is \0.
        if (digits[0] == '0' && digits[1] == 'x')
          yylval->_int = strtol(digits+2, nullptr, 16);
        else if (digits[0] == '0' && digits[1] == 'o')
          yylval->_int = strtol(digits+2, nullptr, 8);
        else
          yylval->_int = strtol(digits, nullptr, 10);
        // Take into account minus sign again:
        if (negative)
          yylval->_int = -yylval->_int;
        return INT_LITERAL;
    }
{float_literal} {
        yylval->_float = atof(yytext);
        return FLOAT_LITERAL;
    }

    /* Skip newlines, whitespace and comments, return any non-matched
     * character. */
[\r\n]|"\r\n" { COUNT_LINE; }
[ \t]+      ;
%.*         ;
.           { return yytext[0]; }

%%
 /* -------------- code section -------------- */
