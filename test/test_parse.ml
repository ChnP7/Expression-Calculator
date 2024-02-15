(***********************************************
 * Test_Parse.ml
 * 
 * Test cases to ensure correct behavior
 * of the Parser functionality 
 *
 **********************************************)
 
 open OUnit2
 open Math.Tokentypes
 open Math.Treetype
 open Math.Lexer
 open Math.Parser

 
 (* Test: Testing a single number, 10 Should be Value (Int 10) *)
 let single_int _ =
	let expected = ([], Value(Int 10)) in
	let result = "10" |> tokenize |> parse in
	assert_equal expected result ~msg:"single_int"
	
 (* Testing a single negative decimal number *)
 let single_neg_float _ = 
	let expected = ([], Value(Float (-16.5))) in
	let result = "-16.5" |> tokenize |> parse in
	assert_equal expected result ~msg:"single_neg_float"
 
 (* Test: 1 + 5 -> Binop(Add, 1, 5) *)
 let op1 _ =
	let expected = ([], Add(Value(Int 1), Value (Int 5))) in
	let result = "1 + 5" |> tokenize |> parse in
	assert_equal expected result ~msg:"op1"
	
 (* Test: More than one add *)
 let multiple_adds _ =
	let expected = ([], Add(Value (Int 1), 
		Add(Value (Int 2), Add(Value (Int 3), Value (Int 4))))) in
	let result = "1 + 2+3+ 4" |> tokenize |> parse in
	assert_equal expected result ~msg:"multiple_adds"
	
(* Test: (5 / 5) - 1 -> Binop (Sub, Binop(Div, 5, 5), 1) *)
 let op2 _ =
	let expected = [Token_Plus] in
	let result = "+" |> tokenize in
	assert_equal expected result ~msg:"op2"
	
	
	
(* For OUnit2 testing *)
let suite =
  "ParseTestList" >::: [
	"single_int" >:: single_int;
	"single_neg_float" >:: single_neg_float;
    "op1" >:: op1;
	"op2" >:: op2;
	"multiple_adds" >:: multiple_adds
  ]

let _ =
  run_test_tt_main suite

