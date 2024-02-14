(***********************************************
 * Test_Lex.ml
 * 
 * Test cases to ensure correct behavior
 * of the Lexer functionality when 
 * converting to tokens
 *
 * Because the lexer is not responsible for 
 * what is correct syntax (e.g. ++2-3*9-//)
 * cases like these should still be translated
 * into to tokens correctly.
 **********************************************)
 
 open OUnit2
 open Math.Lexer
 open Math.Tokentypes
 
 
 (* Helper to print out values 
 let rec print_vals list =
 match list with
 | h::t -> let () = printf " VAL: %s" (string_of_token h) in print_vals t
 | [] -> printf "eggs" *)
 
 
 
 
 
 (* Test: + *)
 let basic1 _ =
	let expected = [Token_Plus] in
	let result = "+" |> tokenize in
	assert_equal expected result ~msg:"basic1"
	
 (* Test: 2+17 *)
 let basic2 _ = 
	let expected = [Token_Int 2; Token_Plus; Token_Int 17] in
	let  result = "2+17" |> tokenize in
	assert_equal expected result ~msg:"basic2"
	
 (* Test: 0 * 7 - 3 + 18 / 4 ^ 10 *)
 let basic3 _ = 
	let expected = 
		[Token_Int 0; Token_Mult; Token_Int 7; Token_Minus; 
		 Token_Int 3; Token_Plus; Token_Int 18; Token_Div;
		 Token_Int 4; Token_Exp; Token_Int 10] in
	let  result = "0 * 7 - 3+18 / 4 ^ 10" |> tokenize in
	assert_equal expected result ~msg:"basic3"
	
 (* Test: -1 *)
 let neg_int_basic _ = 
	let expected = 
		[Token_Int (-1)] in
	let  result = "-1" |> tokenize in
	assert_equal expected result ~msg:"neg_int_basic"	
	
(* Test: -11 --3 --10 *)
 let neg_int _ = 
	let expected = 
		[Token_Int (-11); Token_Minus; Token_Int (-3); Token_Minus; 
		 Token_Int (-10)] in
	let  result = "-11 --3 --10" |> tokenize in
	assert_equal expected result ~msg:"neg_int"
	
 
 (* Test: (5+3)(2+2) *)
 let parenthesis _ = 
	let expected = 
		[Token_LParen; Token_Int 5; Token_Plus; Token_Int 3; 
		 Token_RParen; Token_LParen; Token_Int 2; Token_Plus;
		 Token_Int 2; Token_RParen] in
	let  result = "(5+3)(2+2)" |> tokenize in
	assert_equal expected result ~msg:"parenthesis"
	
	
(* Test: (((3(1*0)))) *)
 let nested_parenthesis _ = 
	let expected = 
		[Token_LParen; Token_LParen; Token_LParen; Token_Int 3; 
		 Token_LParen; Token_Int 1; Token_Mult; Token_Int 0;
		 Token_RParen; Token_RParen; Token_RParen; Token_RParen] in
	let  result = "(((3(1*0))))" |> tokenize in
	assert_equal expected result ~msg:"nested_parenthesis"
	
	
 (* Test: nonsense the parser would not allow, but the lexer should succeed *)
 let nonsense _ = 
	let expected = 
		[Token_Int 5; Token_RParen; Token_Plus; Token_Int (-9); 
		 Token_LParen] in
	let  result = "5)+-9(" |> tokenize in
	assert_equal expected result ~msg:"nonsense"
	
 
 (* Test: correct float tokens *)
 let test_float _ = 
	let expected = 
		[Token_Float (5.9); Token_Div; Token_Float (3.); Token_LParen; 
		 Token_Int 1; Token_RParen] in
	let  result = "5.9/3.0(1)" |> tokenize in
	assert_equal expected result ~msg:"float"
	
(* Test: correct negative float tokens *)
 let test_neg_float _ = 
	let expected = 
		[Token_Float (-0.92); Token_Plus; Token_Float (3.1)] in
	let  result = "-0.92 + 3.1" |> tokenize in
	assert_equal expected result ~msg:"float"
	
	
 (* Test omitting of whitespaces *)
 let whitespaces _ = 
	let expected = [Token_Int 0; Token_Plus; Token_Int 0] in
	let result = "  0  +  0  " |> tokenize in
	assert_equal expected result ~msg:"whitespace:   0  +  0  "
	
	
 (* Test an empty string, should output an empty list since there are no tokens *)
 let empty _ = 
	let expected = [] in
	let result = "" |> tokenize in
	assert_equal expected result ~msg:"empty"
	
	
 (* TODO: Test: String value -> Should raise an exception *)
  let string_fail _ = 
	let result = fun () -> "Nice" |> tokenize in
	assert_raises (InvalidInputException "Invalid input") result ~msg:"string fail"
 
 (* Test: "decimal typo" such as "5." -> Should raise an exception *)
  let decimal_fail _ = 
	let result = fun () -> "5." |> tokenize in
	assert_raises (InvalidInputException "Invalid input") result ~msg:"decimal fail"
 
 (* Test: A decimal without a leading whole number e.g. .78 instead of 0.78 This should succeed. *)
	
	
	
	

	
	
	
(* For OUnit2 testing *)
let suite =
  "TestList" >::: [
    "basic1" >:: basic1;
	"basic2" >:: basic2;
	"basic3" >:: basic3;
	"neg_int_basic" >:: neg_int_basic;
	"neg_int" >:: neg_int;
	"parenthesis" >:: parenthesis;
	"nested_parenthesis" >:: nested_parenthesis;
	"nonsense" >:: nonsense;
	"test_float" >:: test_float;
	"test_neg_float" >:: test_neg_float;
	"whitespaces" >:: whitespaces;
	"empty" >:: empty;
	"string_fail" >:: string_fail;
	"decimal_fail" >:: decimal_fail
  ]

let _ =
  run_test_tt_main suite

