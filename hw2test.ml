type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term]];
     | Term ->
	 [[T"$";T"$"];
          [N Incrop];
	  [T"$"]]
     | Lvalue ->
	 [[T"$"]]
     | Incrop ->
         [[T"$"]])

let test_1 = ((parse_prefix awkish_grammar accept_all []) = None)

let test_2 = ((parse_prefix awkish_grammar accept_all ["$"]) =
    Some ([(Expr, [N Term]); (Term, [N Incrop]); (Incrop, [T "$"])], []))
