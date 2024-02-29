(***********************************************
 * Test_Parse.ml
 * 
 * Test cases to ensure correct behavior
 * of the evaluator.
 *
 **********************************************)
 
 open OUnit2
 open Math.Lexer
 open Math.Parser
 open Math.Eval
 
 
 (* Test a single integer. 1 should evaluate to 1 *)
 let test_int _ = 
	let expected = 1 in
	let result = "1" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"test_int"
 
 (* Test simple addition of two integers *)
 let simple_add _ =
	let expected = 5 in
	let result = "2 + 3" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"simple_add"
	
 (* Multiple adds *)
 let multi_add _ = 
	let expected = 10 in
	let result = "1 + 2 + 3 + 4" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"multi_add"
	
 (* Basic subtract *)
 let basic_sub _ = 
	let expected = 14 in
	let result = "21 - 7" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"basic_sub"
	
 (* Subtract negative *)
 let sub_neg _ = 
	let expected = 4 in
	let result = "1 --3" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"sub_neg"
	
 (* Same as sub_neg, but ensure the same output *)
 let sub_neg2 _ =
	let expected = 4 in
	let result = "1 - (-3)" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"sub_neg2"
 
 (* For OUnit2 testing *)
let suite =
  "ParseTestList" >::: [
	"test_int" >:: test_int;
	"simple_add" >:: simple_add;
	"multi_add" >:: multi_add;
	"basic_sub" >:: basic_sub;
	"sub_neg" >:: sub_neg;
	"sub_neg2" >:: sub_neg2
  ]

let _ =
  run_test_tt_main suite