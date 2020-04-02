let my_subset_test0 = subset [] [1;2;3;4;5]
let my_subset_test1 = subset [1;2;2;2;2] [1;2;3]

let my_equal_sets_test0 = equal_sets [1;1] [1;1;1]
let my_equal_sets_test1 = equal_sets [1;2;2;2;2] [1;2]

let my_set_union_test0 = equal_sets (set_union [1;2;3] [4;5]) [1;2;3;4;5]
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3;5]) [1;2;3;5]
let my_set_union_test2 = equal_sets (set_union [1] []) [1]

let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3;3]) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [2;2;2;2] [3;2]) [2]
let my_set_intersection_test2 =
  equal_sets (set_intersection [2] []) []

let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [1]) [2;3]
let my_set_diff_test1 = equal_sets (set_diff [1;2;2;2] [1;2]) []
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x ) 0= 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x*x) 1 = 1
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.


(* An example grammar for a small subset of Awk.  *)

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let my_filter_reachable_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let my_filter_reachable_test1 =
  filter_reachable (Binop, awksub_rules) = (Binop, [ (Binop, [T"+"]); (Binop, [T"-"])])

let my_filter_reachable_test2 =
  filter_reachable (Incrop, awksub_rules) = (Incrop, [ (Incrop, [T"++"]); (Incrop, [T"--"])])

