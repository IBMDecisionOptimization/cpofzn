include "bin_packing_capa.mzn";

predicate fzn_bin_packing(int: capacity,
                          array[int] of var int: x,
                          array[int] of int: weight) =
  let {
    set of int: idx = lb_array(x)..ub_array(x),
    array[idx] of int: capa = array1d(idx, [capacity|i in idx])
  } in
  bin_packing_capa(capa, x, weight);

