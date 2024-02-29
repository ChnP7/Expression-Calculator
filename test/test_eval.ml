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
	let expected = 1.0 in
	let result = "1" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"test_int"
 
 (* Test simple addition of two integers *)
 let simple_add _ =
	let expected = 5.0 in
	let result = "2 + 3" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"simple_add"
	
 (* Multiple adds *)
 let multi_add _ = 
	let expected = 10.0 in
	let result = "1 + 2 + 3 + 4" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"multi_add"
	
 (* Basic subtract *)
 let basic_sub _ = 
	let expected = 14.0 in
	let result = "21 - 7" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"basic_sub"
	
 (* Subtract negative *)
 let sub_neg _ = 
	let expected = 4.0 in
	let result = "1 --3" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"sub_neg"
	
 (* Same as sub_neg, but ensure the same output *)
 let sub_neg2 _ =
	let expected = 4.0 in
	let result = "1 - (-3)" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"sub_neg2"
	
 (* Test a single floating point *)
 let eval_float _ = 
	let expected = 3.1415 in
	let result = "3.1415" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"eval_float"

 (* Test interation between negative and positive integers and floats *)
 let mixed_types _ = 
	let expected = 20.0 in
	let result = " 10.1 - (-7) + 3 + -0.1" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"mixed_types" 
 
 (* Test order of operations *)
 let eval_pemdas1 _ =
	let expected = 1.0 in
	let result = "1 + 3 * 4 - 12" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"eval_pemdas1"
 
 let eval_pemdas2 _ =
	let expected = 8.5 in
	let result = "3 - 1 * 7 / 2 + 3 ^ 2" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"eval_pemdas2"
 
 let eval_pemdas3 _ =
	let expected = 1.5 in
	let result = "3 * (7 + 1) / (4 - 0) ^ 2" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"eval_pemdas3"
	
 (* Test implicit multiplication when parenthesis are involved *)
 let parenthesis _ = 
	let expected = 5.0 in
	let result = "3(5) - (4 + 1)(2)" |> tokenize |> parse |> eval in
	assert_equal expected result ~msg:"parenthesis"
 
 
 (* For OUnit2 testing *)
let suite =
  "ParseTestList" >::: [
	"test_int" >:: test_int;
	"simple_add" >:: simple_add;
	"multi_add" >:: multi_add;
	"basic_sub" >:: basic_sub;
	"sub_neg" >:: sub_neg;
	"sub_neg2" >:: sub_neg2;
	"eval_float" >:: eval_float;
	"mixed_types" >:: mixed_types;
	"eval_pemdas1" >:: eval_pemdas1;
	"eval_pemdas2" >:: eval_pemdas2;
	"eval_pemdas3" >:: eval_pemdas3;
	"parenthesis" >:: parenthesis
  ]

let _ =
  run_test_tt_main suite