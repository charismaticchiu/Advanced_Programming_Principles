Below are the results of the grading for homework 4, part 1.

## Scores from homework hwk_04_part_1.ml.

+ 3 / 3: Functions are put into the appropriate file: ``homework/hwk_04/hwk_04_part_1.ml``

    File stored correctly in ``homework/hwk_04//hwk_04_part_1.ml``.


The following functions were used in testing:
```
let remove_spaces s = String.lowercase (String.concat (String.split_on_chars s ~on:[' ']))

let remove_spaces_and_compare s1 s2 = (remove_spaces s1) = (remove_spaces s2)
```
If a test case failed for you, consider copying these functions into your
file and experimenting with them to see what went wrong.


+ 5 / 5: ``show_expr (Sub (Let ("x", Add (Const 1, Const 2), Mul (Const 3, Var "x")), Div (Const 4, Const 5)))`` should evaluate to ``"((let x=(1+2) in (3*x))-(4/5))"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Mul (Add (Const 1, Add (Const 2, Const 3)), Add (Add (Const 4, Const 5), Const 6)))`` should evaluate to ``"(1+(2+3))*(4+5+6)"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Sub (Add (Const 1, Const 2), Const 3))`` should evaluate to ``"1+2-3"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Div (Let ("x", Const 1, Var "x"), Let ("y", Const 2, Var "y")))`` should evaluate to ``"(let x=1 in x)/let y=2 in y"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Add (Add (Let ("x", Const 1, Var "x"), Let ("y", Const 2, Var "y")), Let ("z", Const 3, Var "z")))`` should evaluate to ``"(let x=1 in x)+(let y=2 in y)+let z=3 in z"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Let ("x", Let ("y", Const 1, Var "y"), Let ("z", Var "x", Var "z")))`` should evaluate to ``"let x=let y=1 in y in let z=x in z"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Add (Add (Const 1, Const 2), Add (Const 3, Const 4)))`` should evaluate to ``"1+2+(3+4)"``

    Test passed.

+ 8 / 8: ``show_pretty_expr (Sub (Sub (Const 7, Const 1), Sub (Const 5, Const 2)))`` should evaluate to ``"7-1-(5-2)"``

    Test passed.

### Total score

+ 64 / 64: total score for this assignment

