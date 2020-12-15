include "fzn_subcircuit.mzn";

predicate fzn_circuit(array[int] of var int: x) =
    fzn_subcircuit(x) /\ forall(i in index_set(x)) (x[i] != i);
