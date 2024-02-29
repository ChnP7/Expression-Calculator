
open Lexer
open Parser
open Eval

(* Takes a string representing a math expression and evaluates it *)
let evaluate str = 
	let tokenList = tokenize str in
	let ast = parse tokenList in
	(eval ast)
