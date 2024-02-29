(**************************************************
 * Lexer.ml
 * Converts a mathematical input string into a list
 * of tokens for easier management.
 **************************************************)

open Tokentypes
open Helpers
		

let rec tok_helper input pos =
	if pos >= (String.length input) 
		then []
	else
			
		(* Case: minus sign. Since minus and negative share the same symbol, a character will be a minus token if 
		 * 1. The - character is not the first character in the string
		 * 2. The - character comes after a number character [0-9] or RParenthesis )
		 * Otherwise, it will be treated as a negative number symbol *)
		if (Str.string_match (Str.regexp "-")) input pos && pos != 0  
		&& (Str.string_match (Str.regexp "[0-9|)]") input (pos-1)) then
			(Token_Minus) :: (tok_helper input (pos + 1))
			
		(* Case: Floating point *)
		else if (Str.string_match (Str.regexp "-?[0-9]*[.][0-9]+")) input pos then
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
			
		(* Special Case: negating in parenthesis e.g. -(3+3) *)
		else if (Str.string_match (Str.regexp "-(")) input pos then
			(Token_Int (-1)) :: (Token_LParen) :: (tok_helper input (pos + 2))
			
		
		
		(* Case: plus sign *)
		else if (Str.string_match (Str.regexp "\\+")) input pos then 
			(Token_Plus) :: (tok_helper input (pos + 1)) 
			
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
			
		(* Invalid inputs such as strings and such *)
		else raise (InvalidInputException "Invalid input");;

(* Converts all of input into a list of tokens *)
let tokenize input = tok_helper (strip_whitespace input) 0;;
	
