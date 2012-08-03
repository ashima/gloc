open List
open Options

let rec get_flag names default params = match names with
  | []          -> default
  | name::names -> try match assoc name params with
      | "" -> name
      | v  -> v
    with Not_found -> get_flag names default params

let get_strings name default params = 
  let rec go params = match params with
    | [] -> []
    | (pn, v)::rest when pn == name -> v::go rest
    | _::rest -> go rest in
  match go params with
    | [] -> [default]
    | ps -> ps










