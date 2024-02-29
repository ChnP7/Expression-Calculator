(****************************************
 * helpers.ml
 *
 * collection of useful helper functions
 ****************************************)
 
 open Tokentypes
 open Printf
 
 (* Helper: converts tokens to strings *)
 let string_of_token tok =
	match tok with
	| Token_LParen -> "("
	| Token_RParen -> ")"
	| Token_Plus -> "+"
	| Token_Minus -> "-"
	| Token_Div -> "/"
	| Token_Mult -> "*"
	| Token_Exp -> "^"
	| Token_Int i -> string_of_int i
	| Token_Float f -> string_of_float f
	
 
 (* Strips all whitespace from a string *)
 let strip_whitespace input = 
	Str.(global_replace (regexp "[ \\|\t\\|\n]") "" input)
	
	
 (* Gets the next token of a token list *) 
 let lookahead (tokens: token list) =
	match tokens with
	| [] -> None
	| h::_ -> Some h
	
	
 (* Ensures the next token matches the specified token type *)
 let match_token (tokens: token list) token_to_match =
	match tokens with
	| [] -> raise (InvalidInputException("Cannot have an empty token list!"))
	| h::t when h = token_to_match -> t
	| _ -> raise (InvalidInputException("Expected the correct operation"))
	
	
(* Helper to print out token values *)
 let rec print_vals list =
	match list with
	| h::t -> let () = printf " VAL: %s" (string_of_token h) in print_vals t
	| [] -> printf "END" 
	
 (* Exponent function: finds x to the power of p *)
 let rec pow (x: float) p =
	match p with
	| 0 -> 1.0
	| 1 -> x
	| _ -> x *. (pow x (p-1))
	
	
