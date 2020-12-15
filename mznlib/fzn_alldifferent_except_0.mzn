%-----------------------------------------------------------------------------%
% Constrains the elements of the array 'vs' to be all different except those
% elements that are assigned the value 0.
%-----------------------------------------------------------------------------%

predicate fzn_alldifferent_except_0(array[int] of var int: vs) =
%
% Improve this test to intmin + length(vs)
%
  assert(forall(i in index_set(vs)) (lb(vs[i]) >= 0),
  "all_different_except_0 not supported on variables with negative lower bounds",
  let {
    int: top = max(i in index_set(vs)) (ub(vs[i])),
    int: bot = min(i in index_set(vs)) (-i),
    int: loidx = min (i in index_set(vs)) (i),
    int: hiidx = max (i in index_set(vs)) (i),
    array[loidx..hiidx] of var bot..top: shadow,
  } in
  (forall (i in index_set(vs))
    (shadow[i] in ({-i} union (lb(vs[i])..ub(vs[i]))))
  ) /\
  (forall (i in index_set(vs))
    ((shadow[i] == -i /\ vs[i] == 0) \/ (shadow[i] == vs[i]))
  ) /\
  all_different(shadow))

%predicate fzn_alldifferent_except_0(array[int] of var int: vs) =
%    forall(i, j in index_set(vs) where i != j) (
%       (vs[i] != 0 /\ vs[j] != 0) -> vs[i] != vs[j]
%    );
