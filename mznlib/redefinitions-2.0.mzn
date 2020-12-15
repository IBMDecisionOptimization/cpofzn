% Minizinc since version 2.0 has the following predicates. To show that we can
% handle them we need to list them in this file.

predicate bool_clause_reif(array[int] of var bool: as, array[int] of var bool: bs, var bool: b);
predicate array_int_maximum(var int: m, array[int] of var int: x);
predicate array_float_maximum(var float: m, array[int] of var float: x);
predicate array_int_minimum(var int: m, array[int] of var int: x);
predicate array_float_minimum(var float: m, array[int] of var float: x);

% TODO/2:
% We can redefine array_var_int_element_nonshifted and similar predicates here.
% Right now, minizinc shifts the index to start by 1 and then we shift it again
% to start by 0 (by prefixing the array by a dummy value). We could compute the
% shift ourselves here (the same way we already compute the shift in our
% fzn_subcircuit.mzn). And then in C++ we can decide what to do about the
% shift: if it is small then prefix the array otherwise substract the shift
% from the index.
