predicate cpo_inverse(int: fshift, array[int] of var int: f,
                      int: invfshift, array[int] of var int: invf);

predicate fzn_inverse(array[int] of var int: f, array[int] of var int: invf) =
  cpo_inverse(min(index_set(invf)), f, min(index_set(f)), invf);
