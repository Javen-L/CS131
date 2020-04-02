let accept_all string = Some string;;
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

type the_nonterminals =
  |Sentence |Subj| Obj | Verb |Person | Animal ;;

let the_grammar =
  (Sentence,
   function
 	| Sentence -> 
 		[	[N Subj; N Verb; N Obj];
 			[N Subj; N Verb];
 			[N Subj; T "says"; N Sentence]	
 		]

 	| Subj ->
 		[	[N Person];
 			[N Animal]
 		]
 	| Person ->
 		[	[T "Nick"]; 
 			[T "Dodd"]; 
 			[T "Goerge"]

 		]
 	| Animal ->
 		[	[T "tiger"];
 			[T "bunny"]
 		]
 	| Verb ->
 		[	[T "eats"];
 			[T "buys"]	
 		]
 	|Obj ->
 		[	[T "book"];
 			[T "apple"]
 		]
 	);;






let make_matcher_test = 
  ((make_matcher the_grammar accept_all 
  	["Nick"; "says"; "tiger";"eats";"apple";"bunny"]) = Some ["bunny"]);;

let make_parser_test = 
	match (make_parser the_grammar 
	["Nick"; "says"; "tiger";"eats";"apple"]) with 
	|Some tree->
	parse_tree_leaves tree = 
	["Nick"; "says"; "tiger";"eats";"apple"]	
	|_->false;;
