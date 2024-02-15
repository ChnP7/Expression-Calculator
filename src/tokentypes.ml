exception InvalidInputException of string

type token = 
	| Token_LParen
	| Token_RParen
	| Token_Plus
	| Token_Minus
	| Token_Div
	| Token_Mult
	| Token_Exp
	| Token_Int of int
	| Token_Float of float;;