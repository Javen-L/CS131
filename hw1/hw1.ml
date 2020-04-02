open List;;

let rec subset a b = match a with
	|[]->true
	|_ ->if mem (hd a) b then subset (tl a) b else false;;

let equal_sets a b = 
	if subset a b && subset b a then true else false;;

let rec set_union a b = match a with
	|[]->b
	|_ -> if mem (hd a) b then set_union (tl a) b else (hd a):: 
	set_union (tl a) b;;

let rec set_intersection a b =  match a with
	|[]->[]
	|_-> if mem (hd a) b then (hd a):: (set_intersection (tl a) b) 
		else set_intersection (tl a) b;;

let rec set_diff a b = match a with
	|[]->[]
	|_-> if mem (hd a) b then set_diff (tl a) b 
		else (hd a)::set_diff (tl a) b;;

let rec computed_fixed_point eq f x = 
	if eq x (f x) then x else computed_fixed_point eq f (f x);;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* hint : use equal_sets and computed_fixed_point*)



let t_f_e (a,b) = a;;
let t_s_e (a,b) = b;;

(*note: inputs are tuples not lists*)
let equal_second_elem_sets a b = 
	equal_sets (t_s_e a) (t_s_e b);;

(*let test_non_terminal a = match a with 
	| N n -> true
	| T t -> false;;*)

(*let filter_non_terminal a = filter test_non_terminal a;;*)

(*let rec filter_non_terminal a = match a with 
	| [] -> []
	| _ -> if test_non_terminal (hd a) then (hd a)::filter_non_terminal (tl a)
			else filter_non_terminal (tl a);;*)

let rec filter_non_terminal a = match a with 
	[]->[]
	| N head::tail -> head::filter_non_terminal tail
	| T head::tail -> filter_non_terminal tail;;


(*a:rules, a list of tuples of (symbol,symbol list)
  b: reachable symbols, a list*)

 (*todo don't change a*)
let rec grshelper (a,b) = match (a,b) with 
	| ([],b)-> (a,b)
	| _-> if mem (t_f_e (hd a)) b
		then grshelper ((tl a),(b @ (filter_non_terminal (t_s_e (hd a))))) 
		else grshelper ((tl a) ,b) ;;

let get_reachable_symbols (a,b) = 
	(a,t_s_e (grshelper (a,b)));;


(*keep only the reachable rules in grammar g
    parameters:
        g: a pair, first element is a symbol, second element is a list of rules
    return:
        grammar after filtered the symbols
    sample usage: filter_reachable(sample_grammar)*)

let rec filter_reachable g = match g with (symbol,rules)->
	let (a,b) = computed_fixed_point equal_second_elem_sets get_reachable_symbols (rules,[symbol]) in
	let filtered_rules = filter (fun x -> mem (t_f_e x) b )  rules in
	(symbol, filtered_rules);;
