datatype expr = 
  IConst of int 
  | Plus of expr * expr 
  | Minus of expr * expr
  | Multi of expr * expr
  | Div of expr * expr
  | Max of expr * expr
  | Min of expr * expr
  | Eq of expr * expr
  | Gt of expr * expr
;

fun eval (IConst i) = i
  | eval (Plus(e1, e2)) = (eval e1) + (eval e2)
  | eval (Minus(e1, e2)) = (eval e1) - (eval e2)
  | eval (Multi(e1, e2)) = (eval e1) * (eval e2)
  | eval (Div(e1, e2)) = 
      if (eval e2 = 0) then 
        0
      else
        (eval e1) div (eval e2)
  | eval (Max(e1, e2)) =
      if ((eval e1) > (eval e2)) then
        (eval e1)
      else
        (eval e2)
  | eval (Min(e1,e2)) =
      if ((eval e1) < (eval e2)) then
        (eval e1)
      else
        (eval e2)
  | eval (Eq(e1,e2)) = 
      if ((eval e1) = (eval e2)) then
        1
      else
        0
    
  | eval (Gt(e1, e2)) =
      if ((eval e1) > (eval e2)) then
        1
      else
        0
  ;


exception AssertionError
fun assert (e1,expected) =
   if e1 <> expected then 
      raise AssertionError
   else
      true
;

(* Multi *)
assert( eval(Minus(Plus(IConst 23, IConst 5), IConst 6)), 22); 
assert( eval(Multi(Plus(IConst 23, IConst 5), IConst ~6)), ~168);
assert( eval(Multi(Plus(IConst 23, IConst 5), IConst 1)), 28);
assert( eval(Multi(Plus(IConst 23, IConst 5), IConst 0)), 0);

(* Div *)
assert( eval(Div(Plus(IConst 25, IConst 5), IConst 5)), 6); 
assert( eval(Div(Plus(IConst ~25, IConst ~5), IConst ~5)), 6); 
assert( eval(Div(Plus(IConst ~25, IConst ~5), IConst 5)), ~6); 
assert( eval(Div(Plus(IConst 23, IConst 5), IConst 6)), 4); 
assert( eval(Div(Plus(IConst ~23, IConst ~5), IConst ~6)), 4); 
assert( eval(Div(Plus(IConst 23, IConst 5), IConst 1)), 28);
assert( eval(Div(Plus(IConst 23, IConst 5), IConst 0)), 0);

(* Div (+ e -) *)
assert( eval(Div(Plus(IConst 25, IConst 5), IConst ~5)), ~6);
assert( eval(Div(Plus(IConst 23, IConst 5), IConst ~6)), ~4);
assert( eval(Div(Plus(IConst ~23, IConst ~5), IConst 6)), ~4);

(* Max *)
assert( eval(Max(Plus(IConst 23, IConst 5), IConst 6)), 28);
assert( eval(Max(IConst 6,Plus(IConst 23, IConst 5))), 28);
assert( eval(Max(Plus(IConst 23, IConst 5), Plus(IConst 23, IConst 5))), 28);
assert( eval(Max(IConst ~1, IConst 1)), 1);
assert( eval(Max(IConst ~1, IConst ~11)), ~1);
assert( eval(Max(IConst 0, IConst 1)), 1);
assert( eval(Max(IConst 0, IConst ~1)), 0);

(* Min *)
assert( eval(Min(Plus(IConst 23, IConst 5), IConst 6)), 6);
assert( eval(Min(IConst 6,Plus(IConst 23, IConst 5))), 6);
assert( eval(Min(Plus(IConst 23, IConst 5), Plus(IConst 23, IConst 5))), 28);
assert( eval(Min(IConst ~1, IConst 1)), ~1);
assert( eval(Min(IConst ~1, IConst ~11)), ~11);
assert( eval(Min(IConst 0, IConst 1)), 0);
assert( eval(Min(IConst 0, IConst ~1)), ~1);

(* Eq *)
assert( eval(Eq(Plus(IConst 23, IConst 5), IConst 28)), 1);
assert( eval(Eq(Plus(IConst 23, IConst 5), IConst ~28)), 0);

(* Gt *)
assert( eval(Gt(Plus(IConst 23, IConst 5), IConst 6)), 1); 
assert( eval(Gt(IConst 6,Plus(IConst 23, IConst 5))), 0); 
assert( eval(Gt(Plus(IConst 23, IConst 5), Plus(IConst 23, IConst 5))), 0);
assert( eval(Gt(IConst ~1, IConst 1)), 0);
assert( eval(Gt(IConst ~1, IConst ~11)), 1);
assert( eval(Gt(IConst 0, IConst 1)), 0);
assert( eval(Gt(IConst 0, IConst ~1)), 1);
"TESTES CONCLUIDOS COM SUCESSO!"