type op = 
	  Add 
	| Sub 
	| Mult 
	| Div 
	| Exp

and value = 
	| Int of int
	| Float of float
	
and expr = 
	| Value of value
	| Binop of op * expr * expr