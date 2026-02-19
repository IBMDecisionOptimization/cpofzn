predicate cpo_table_bool_reif(array[int] of var int: x, array[int] of bool: t, var bool: b);

predicate fzn_table_bool_reif(array[int] of var bool: x, array[int, int] of bool: t, var bool: b) = cpo_table_bool_reif(x, array1d(t), b);
%predicate fzn_table_bool_reif(array[int] of var bool: x, array[int] of bool: t, var bool: b);
