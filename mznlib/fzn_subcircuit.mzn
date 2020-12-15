predicate cpo_subcircuit(int: shift, array[int] of var int: x);

predicate fzn_subcircuit(array[int] of var int: x) =
  cpo_subcircuit(min(index_set(x)), x);
