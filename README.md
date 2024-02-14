## Expression and Equation Solver
A mathematical expression and equation solver. 
For now, it currently limited to order of operations arithmetic expressions including addition, subtraction, multiplication, division, Exponents, and anything enclosed in parenthesis.
Negative and positive integers (whole numbers) are supported, as well as floating point (decimal) numbers.

This includes a lexer and parser that are used to take apart individual tokens from the string input to form a structure that can be evaluated in the correct order.

Written in OCaml and build using Dune. The test modules for this project use OUnit to test and assert correct expected outputs.

## Example uses
Input: `2+7` Output: `9`

Input: `2(5-3)` Output: `4`

Input: `(7-6)/(10-9) + 13` Output: `14`

Input: `-12.6 --3.1` Output: `-9.5`

Input: `2^3+0 - 2.5` Output: `5.5`

## Future plans
- Add Support for simplifying expressions and solving equations with single variables e.g. `5x + 3x` will return `8x`
- Add support for graphing basic equations
- Add support for fraction simplification
- Add support for keeping numbers in fraction form instead of decimal if desired e.g. `0.50` can be output as `1/2` if the original input asked to simplify `5/10`