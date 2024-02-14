(****************************************
 * helpers.ml
 *
 * collection of useful helper functions
 ****************************************)
 
 open Tokentypes
 
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