

type value = 
	| Int of int
	| Float of float
	
and expr = 
	| Value of value
	| Add of expr * expr
	| Sub of expr * expr
	| Mult of expr * expr
	| Div of expr * expr
	| Exp of expr * expr