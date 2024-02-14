(**************************************************
 * Lexer.ml
 * Converts a mathematical input string into a list
 * of tokens for easier management.
 **************************************************)

open Tokentypes


exception InvalidInputException of string


let rec tok_helper input pos =
	if pos >= (String.length input) 
		then []
	else
		
		(* Case: Floating point *)
		if (Str.string_match (Str.regexp "-?[0-9]+[.][0-9]+")) input pos then
			let tok = (Str.matched_string input) in
			let len = (String.length tok) in
			let num = float_of_string tok in
			(Token_Float num) :: (tok_helper input (pos + len))
		
		(* Case: Integer *)
		else if (Str.string_match (Str.regexp "-?[0-9]+")) input pos then
			let tok = (Str.matched_string input) in
			let len = (String.length tok) in
			let num = int_of_string tok in
			(Token_Int num) :: (tok_helper input (pos + len))
		
		(* Case: plus sign *)
		else if (Str.string_match (Str.regexp "\\+")) input pos then 
			(Token_Plus) :: (tok_helper input (pos + 1)) 
			
		(* Case: minus sign *)
		else if (Str.string_match (Str.regexp "-")) input pos then
			(Token_Minus) :: (tok_helper input (pos + 1))
			
		(* Case: multiplication sign *)
		else if (Str.string_match (Str.regexp "\\*")) input pos then
			(Token_Mult) :: (tok_helper input (pos + 1))
			
		(* Case: division sign *)
		else if (Str.string_match (Str.regexp "/")) input pos then
			(Token_Div) :: (tok_helper input (pos + 1))
			
		(* Case: exponent sign *)
		else if (Str.string_match (Str.regexp "\\^")) input pos then
			(Token_Exp) :: (tok_helper input (pos + 1))
			
		(* Case: left parenthesis *)
		else if (Str.string_match (Str.regexp "(")) input pos then
			(Token_LParen) :: (tok_helper input (pos + 1))
			
		(* Case: right parenthesis *)
		else if (Str.string_match (Str.regexp ")")) input pos then
			(Token_RParen) :: (tok_helper input (pos + 1))	
		
		(* Discard whitespace if encountered in pos*)
		else if (Str.string_match (Str.regexp "[ \\|\t\\|\n]")) input pos then 
			(tok_helper input (pos+1))
			
		(* Invalid inputs such as strings and such *)
		else raise (InvalidInputException "Invalid input");;
		

(* Converts all of input into a list of tokens *)
let tokenize input = tok_helper input 0;;
	
