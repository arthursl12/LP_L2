type  Num = int;
type  Var = string;
datatype  Aexpr =
  N of Num
  | V of Var
  | Plus of Aexpr * Aexpr
  | Mult of Aexpr * Aexpr
  | Minus  of Aexpr * Aexpr;

datatype  Bexpr =
  True
  | False
  | Eq of  Aexpr * Aexpr
  | Leq of  Aexpr * Aexpr
  | Not of  Bexpr
  | And of  Bexpr * Bexpr;

datatype  Stm =
  Assign  of Var * Aexpr
  | Skip
  | Comp of Stm * Stm
  | If of  Bexpr * Stm * Stm
  | While of  Bexpr * Stm
  | Repeat of  Stm * Bexpr;


fun  evalN n : Num = n

exception  FreeVar;
fun  lookup  [] id = raise  FreeVar
   | lookup  ((k:string , v)::l) id = if id = k then v else  lookup l id;

fun  evalA (N n) _ = evalN n
  |  evalA (V x) s = lookup s x
  |  evalA (Plus(e1, e2)) s = (evalA  e1 s) + (evalA  e2 s)
  |  evalA (Mult(e1, e2)) s = (evalA  e1 s) * (evalA  e2 s)
  |  evalA (Minus(e1, e2)) s = (evalA  e1 s) - (evalA  e2 s);

fun  evalB  True _ = true
  |  evalB  False _ = false
  |  evalB (Eq(a1, a2)) s = (evalA  a1 s) = (evalA  a2 s)
  |  evalB (Leq(a1, a2)) s = (evalA  a1 s)  <= (evalA  a2 s)
  |  evalB (Not b) s = not (evalB b s)
  |  evalB (And(b1, b2)) s = (evalB  b1 s) andalso (evalB  b2 s);

fun  evalStm (stm : Stm) (s : (string * int) list) : (string *int) list = 
  case  stm of
    (Assign(x, a)) => (x, evalA a s)::s
  | (Skip) => s
  | (Comp(stm1 , stm2)) => evalStm  stm2 (evalStm  stm1 s)
  | (If(b, stm1 , stm2)) =>
      if (evalB b s) then  evalStm  stm1 s else  evalStm  stm2 s
  | (While(b, stm)) =>   
      if (evalB b s) then
         evalStm (While(b, stm)) (evalStm stm s)
      else
         s
  | (Repeat(stm, b)) =>
      let
        val s' = (evalStm stm s)
      in
        if not(evalB b s') then
          evalStm (Repeat(stm, b)) s'
        else
          s'
      end
;


exception AssertionError
fun assert (e1: ''a, expected: ''a) =
      if e1 <> expected then 
        raise AssertionError
      else
        true
;

(*
  x := 1;
  y := x+5;
  if y <= 6 then z := y else z := y*2
*)
val p1 =
  Comp(
    Assign ("x", N 1),
    Comp(
      Assign("y", Plus(V "x", N 5)),
      If(Leq(V "y", N 6),
          Assign("z", V "y"),
          Assign("z", Mult (V "y", N 2))
      )
    )
  );

assert(lookup (evalStm p1 []) "z", 6);
assert(lookup (evalStm p1 [("x",5)]) "z", 6);

(*
  x := 2;
  y := x+5;
  if y <= 6 then z := y else z := y*2
*)
val p3 =
  Comp(
    Assign ("x", N 2),
    Comp(
      Assign("y", Plus(V "x", N 5)),
      If(Leq(V "y", N 6),
          Assign("z", V "y"),
          Assign("z", Mult (V "y", N 2))
      )
    )
  );

assert(lookup (evalStm p3 []) "z", 14);
assert(lookup (evalStm p3 [("x",5)]) "z", 14);

(*
  x := 1;
  while x <= 4 do x := x + 1
*)
val p2 =
  Comp(
    Assign ("x", N 1),
    While(
      Leq(V "x", N 4),
      Assign("x", Plus(V "x", N 1))
    )
  );
assert(lookup (evalStm p2 []) "x", 5);

(*
  x := 1;
  repeat x := x+1 until x > 4;
  ==> x = 4
*)
val p4 =
  Comp(
    Assign ("x", N 1),
    Repeat (
      Assign("x", Plus(V "x", N 1)),
      Not(Leq(V "x", N 4))
    )
  );
assert(lookup (evalStm p4 []) "x", 5);
assert(lookup (evalStm p4 [("x",5)]) "x", 5);

(*
  x := 3;
  repeat x := x+1 until x > 4;
  ==> x = 5
*)
val p5 =
  Comp(
    Assign ("x", N 3),
    Repeat (
      Assign("x", Plus(V "x", N 1)),
      Not(Leq(V "x", N 4))
    )
  );

assert(lookup (evalStm p5 []) "x", 5);
assert(lookup (evalStm p5 [("x",5)]) "x", 5);

(*
  x := 4;
  repeat x := x+1 until x > 4;
  ==> x = 5
*)
val p6 =
  Comp(
    Assign ("x", N 4),
    Repeat (
      Assign("x", Plus(V "x", N 1)),
      Not(Leq(V "x", N 4))
    )
  );
assert(lookup (evalStm p6 []) "x", 5);
assert(lookup (evalStm p6 [("x",5)]) "x", 5);


(*
  x := 5;
  repeat x := x+1 until x > 4;
  ==> x = 6
*)
val p7 =
  Comp(
    Assign ("x", N 5),
    Repeat (
      Assign("x", Plus(V "x", N 1)),
      Not(Leq(V "x", N 4))
    )
  );

assert(lookup (evalStm p7 []) "x", 6);
assert(lookup (evalStm p7 [("x",5)]) "x", 6);
"Testes Concluidos!"