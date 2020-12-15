/*
 CP Optimizer does not support set variables. Convert them into arrays of bool
 variables using standard MiniZinc library:
*/
include "nosets.mzn";

/*
 CP Optimizer does not support trigonometric and hyperbolic functions.
 */
predicate float_acos(var float: a, var float: b) = abort("The builtin float_acos is not supported by CP Optimizer.");
predicate float_acosh(var float: a, var float: b) = abort("The builtin float_acosh is not supported by CP Optimizer.");
predicate float_asin(var float: a, var float: b) = abort("The builtin float_asin is not supported by CP Optimizer.");
predicate float_asinh(var float: a, var float: b) = abort("The builtin float_asinh is not supported by CP Optimizer.");
predicate float_atan(var float: a, var float: b) = abort("The builtin float_atan is not supported by CP Optimizer.");
predicate float_atanh(var float: a, var float: b) = abort("The builtin float_atanh is not supported by CP Optimizer.");
predicate float_cos(var float: a, var float: b) = abort("The builtin float_cos is not supported by CP Optimizer.");
predicate float_cosh(var float: a, var float: b) = abort("The builtin float_cosh is not supported by CP Optimizer.");
predicate float_sin(var float: a, var float: b) = abort("The builtin float_sin is not supported by CP Optimizer.");
predicate float_sinh(var float: a, var float: b) = abort("The builtin float_sinh is not supported by CP Optimizer.");
predicate float_tan(var float: a, var float: b) = abort("The builtin float_tan is not supported by CP Optimizer.");
predicate float_tanh(var float: a, var float: b) = abort("The builtin float_tanh is not supported by CP Optimizer.");
