let rec get_all_t gram1 ele = match gram1 with
| [] -> []
| (lhs, rhs)::t -> if lhs = ele 
    then rhs :: (get_all_t t ele)
    else get_all_t t ele

let convert_grammar = function (first_symbol, grammar) ->
    (first_symbol, (get_all_t grammar))

let rec matcher gram nt acc der frag =
    let rec check_rhs gram rhs acc der frag = match frag with
    | [] -> if rhs = [] 
        then acc der frag 
        else None
    | h::t -> if rhs = [] 
        then acc der frag 
        else match rhs with
        | (N x) :: n_tail -> (matcher gram x (check_rhs gram n_tail acc) der frag)
        | (T y) :: t_tail -> if h = y 
            then (check_rhs gram t_tail acc der t)
            else None
    in
        let rec check_rest gram nt rhs acc der frag = if rhs = [] 
            then None 
            else match rhs with
            | h::t -> match (check_rhs gram h acc (der@[(nt, h)]) frag) with
                | Some(a, b) -> Some(a, b)
                | None -> (check_rest gram nt t acc der frag)
        in
            (check_rest gram nt (gram nt) acc der frag)

let rec parse_prefix (symbol, gram) acc frag =
    (matcher gram symbol acc [] frag)

