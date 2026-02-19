predicate cpo_table_int(array[int] of var int: x, array[int] of int: t);

%predicate fzn_table_int(array[int] of var int: x, array[int, int] of int: t);
%predicate fzn_table_int(array[int] of var int: x, array[int, int] of int: t) = table(x, t);
predicate fzn_table_int(array[int] of var int: x, array[int, int] of int: t) = cpo_table_int(x, array1d(t));
