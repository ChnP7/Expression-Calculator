(***********************************************
 * Test_Parse.ml
 * 
 * Test cases to ensure correct behavior
 * of the Parser functionality 
 *
 **********************************************)
 
 open OUnit2
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
 
 (* Test: 1 + 5 -> Add(1, 5) *)
 let op1 _ =
	let expected = ([], Add(Value(Int 1), Value (Int 5))) in
	let result = "1 + 5" |> tokenize |> parse in
	assert_equal expected result ~msg:"op1"
	
 (* Test: More than one add *)
 let multiple_adds _ =
	let expected = ([], 
	Add (
		Add(
			Add(
				Value(Int 1),
				Value(Int 2)
			),
			Value(Int 3)
		),
		Value(Int 4)
	)) in
	let result = "1 + 2+3+ 4" |> tokenize |> parse in
	assert_equal expected result ~msg:"multiple_adds"

	
	
 (* Test order of operations. AST must have Mult(3*4) bottom most of tree *)
 let pemdas1 _ = 
	let expected = ([],
	Sub(
		Add(
			Value (Int 1),
			Mult(
				Value (Int 3),
				Value (Int 4)
			)
		),
		Value (Int 12)
	)) in
	let result = "1 + 3 * 4 - 12" |> tokenize |> parse in
	assert_equal expected result ~msg:"pemdas1"
	
 (* Another order of operations test, with exponent and div included *)
 let pemdas2 _ = 
	let expected = ([],
	Add(
		Sub (
			Value (Int 3),
			Div (
				Mult (
					Value (Int 1),
					Value (Int 7)
				),
				Value (Int 16)
			)
		),
		Exp (
			Value (Int 3),
			Value (Int 2)
		)
	)) in
	let result = "3 - 1 * 7 / 16 + 3 ^ 2" |> tokenize |> parse in
	assert_equal expected result ~msg:"pemdas2"
	
 (* Test minus paired with a negative number *)
 let minus_neg _ = 
	let expected = ([],
	Sub(
		Value (Int 1),
		Value (Int (-2))
	)) in
	let result = "1 -- 2" |> tokenize |> parse in
	assert_equal expected result ~msg:"minus negative"
	
	
 (* Test correct ordering of values with same priority. Should go left -> right *)
 let left_to_right _ = 
	let expected = ([],
	Sub(
		Sub(
			Sub(
				Sub(
					Sub(
						Value (Int 5),
						Value (Int 4)
					),
					Value (Int 3)
				),
				Value (Int 2)
			),
			Value (Float 1.1)
		),
		Value (Int 0)
	)) in
	let result = "5 - 4 - 3 - 2 - 1.1 - 0" |> tokenize |> parse in
	assert_equal expected result ~msg:"left_to_right"
	
 (* Test basic exponent parsing *)
 let exp _ = 
	let expected = ([],
	Add(
		Value (Int 1),
		Exp (
			Value (Int 2),
			Value (Int 5)
		)
	)) in
	let result = "1 + 2^5" |> tokenize |> parse in
	assert_equal expected result ~msg:"exp"
	
 
	
	
	
(* For OUnit2 testing *)
let suite =
  "ParseTestList" >::: [
	"single_int" >:: single_int;
	"single_neg_float" >:: single_neg_float;
    "op1" >:: op1;
	"multiple_adds" >:: multiple_adds;
	"pemdas1" >:: pemdas1;
	"minus_neg" >:: minus_neg;
	"left_to_right" >:: left_to_right;
	"exp" >:: exp;
	"pemdas2" >:: pemdas2
  ]

let _ =
  run_test_tt_main suite

