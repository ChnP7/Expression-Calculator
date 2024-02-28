(**********************************************
 * parser.ml
 *
 * Takes generated tokens and ensures correct 
 * syntax. 
 *
 * Following order of operations precedence:
 * add_sub calls mult_div before doing anything else
 * mult_div calls exp before anything else
 * exp calls parse_vals before anything else
 * Then the recursive calls come back 
 * value -> exp -> mult_div -> add_sub
 *
 ***********************************************)
 
 open Tokentypes
 open Treetype
 open Helpers
	
 
 let rec parse tokens = 
	 parse_add_sub tokens
	 
 and parse_add_sub tokens = 
	let (tail, e1) = parse_mult_div tokens in (*Evaluate left-hand side first*)
	match lookahead tail with (* Determine op: if add or sub *)
	| Some (Token_Plus) -> 	let tail2 = match_token tail Token_Plus in (* tail2 = Tok_Plus removed (it was expected token) *)
							let (tail3, e2) = parse_add_sub tail2 in (* now eval right hand *)
							(tail3, Add(e1, e2)) (* Return (remaining tokens, Add(x + y))) *)
							
	| Some (Token_Minus) -> let tail2 = match_token tail Token_Minus in
							let (tail3, e2) = parse_add_sub tail2 in
							(tail3, Sub(e1, e2))
							
	| _ -> (tail, e1)
	
 and parse_mult_div tokens = 
	let (tail, e1) = parse_vals tokens in 
	match lookahead tail with 
	| Some (Token_Mult) -> 	let tail2 = match_token tail Token_Mult in
							let (tail3, e2) = parse_mult_div tail2 in
							(tail3, Mult(e1, e2))
							
	| Some (Token_Div) -> 	let tail2 = match_token tail Token_Div in
							let (tail3, e2) = parse_mult_div tail2 in
							(tail3, Div(e1, e2))
							
	| _ -> (tail, e1)
 
 and parse_vals tokens = 
	match lookahead tokens with
	| Some (Token_Int i) -> let tail = match_token tokens (Token_Int i) in
							(tail, Value(Int i))
	| Some (Token_Float f) -> 	let tail = match_token tokens (Token_Float f) in
								(tail, Value(Float f))
	| _ -> raise (InvalidInputException "Expected a integer or decimal")
	

	

 
 