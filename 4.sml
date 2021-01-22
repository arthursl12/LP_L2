datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = 
  IConst of int 
  | Op1 of UnOp * Sexpr 
  | Op2 of BinOp * Sexpr* Sexpr;

fun simplify (Op1(Not, Op1(Not,expr))) = simplify(expr) (* not(not e) *)
  | simplify (Op2(Or, expr1, expr2)) =  (* e or e = e *)
      if (simplify(expr1) = simplify(expr2)) then
        simplify(expr1)
      else
        Op2(Or, simplify(expr1), simplify(expr2))
  | simplify (Op2(Add, expr1, expr2)) = (* Adição *)
      let
        val s1 = simplify(expr1)
        val s2 = simplify(expr2)
      in
        if (s1 = IConst 0) then (* 0 + e = e *) 
          s2
        else if (s2 = IConst 0) then (* e + 0 = e *) 
          s1
        else
          Op2(Add, s1, s2)
        end
  | simplify (Op2(Sub, expr1, expr2)) = (* Subtração *)
      if (simplify(expr1) = simplify(expr2)) then (* e - e = 0 *) 
        IConst 0
      else if (simplify(expr2) = IConst 0) then (* e - 0 = e *)
        simplify(expr1)
      else
        Op2(Sub, simplify(expr1), simplify(expr2))
  | simplify (Op2(Mul, expr1, expr2)) = (* Multiplicação *)
      let
        val s1 = simplify(expr1)
        val s2 = simplify(expr2)
      in  
        if s1 = IConst 1 then (* 1 * e = e *)
          s2
        else if s2 = IConst 1 then (* e * 1 = e *)
          s1
        else if (s1 = IConst 0 orelse s2 = IConst 0) then (* e * 0 = 0 * e = 0 *)
          IConst 0
        else
          Op2(Mul, s1, s2)
      end
  | simplify (Op1(t, expr)) = Op1(t, simplify(expr)) (* Simplificar operando unário *)
  | simplify (Op2(t, expr1, expr2)) = Op2(t, simplify(expr1), simplify(expr2)) (* Simplificar operandos *)
  | simplify (IConst c) = IConst c  (* Não podemos simplificar mais uma constante *)
;

(*
==CHECKLIST==
0 + e (OK)
e + 0 (OK)
e - 0 (OK)
1 * e
e * 1
0 * e
e * 0
e - e (OK)
e or e (OK)
not(not e) (OK)
*)

exception AssertionError
fun assert (e1: ''a, expected: ''a) =
      if e1 <> expected then 
        raise AssertionError
      else
        true
;

(* not(not(3>0)) = 3>0 *)
assert( simplify( Op1(Not, Op1(Not, Op2(Gt, IConst 3, IConst 0)))), 
        Op2(Gt, IConst 3, IConst 0));

(* not(3>0) = not(3>0) *)
assert( simplify( Op1(Not, Op2(Gt, IConst 3, IConst 0))), 
        Op1(Not, Op2(Gt, IConst 3, IConst 0)));

(* (not(not(3>0))) or (not(not(3>0))) = 3>0 *)
assert( simplify(
  Op2(Or,
      Op1(Not, Op1(Not, Op2(Gt, IConst 3, IConst 0))),
      Op1(Not, Op1(Not, Op2(Gt, IConst 3, IConst 0)))
      )
  ),
  Op2(Gt, IConst 3, IConst 0));

(* (not(not(3>1))) or (not(3>0)) = 3>1 or not(3>0) *)
assert( simplify(
  Op2(Or,
      Op1(Not, Op1(Not, Op2(Gt, IConst 3, IConst 1))),
      Op1(Not, Op2(Gt, IConst 3, IConst 0))
      )
  ),
  Op2(Or,
      Op2(Gt, IConst 3, IConst 1),
      Op1(Not, Op2(Gt, IConst 3, IConst 0))
      )
  );

(* (not(not(3>1))) or (3>1)) = 3>1 *)
assert( simplify(
  Op2(Or,
      Op1(Not, Op1(Not, Op2(Gt, IConst 3, IConst 1))),
      Op2(Gt, IConst 3, IConst 1)
      )
  ),
  Op2(Gt, IConst 3, IConst 1)
  );

(* 0 + ((5+0)+0) = 5 *)
assert( simplify(
  Op2(Add,
      IConst 0,
      Op2(Add,
          Op2(Add,
              IConst 5,
              IConst 0
          ),
          IConst 0
      )
  )),
  IConst 5
);

(* (0 + (7 - 0)) - ((5-0)-0) = 7 - 5 *)
assert( simplify(
  Op2(Sub,
      Op2(Add,
          IConst 0,
          Op2(Sub,
              IConst 7,
              IConst 0
          )
      ),
      Op2(Sub, 
          Op2(Sub,
              IConst 5,
              IConst 0
          ),
          IConst 0
      )
  )),
  Op2(Sub,IConst 7, IConst 5));

(* (0 + (5 - (3-0)) - (((5+0) - 3)-0) = 0 *)
assert( simplify(
  Op2(Sub,
      Op2(Add,
          IConst 0,
          Op2(Sub,
              IConst 5,
              Op2(Sub,
                  IConst 3,
                  IConst 0
              )
          )
      ),
      Op2(Sub, 
          Op2(Sub,
              Op2(Add,
                  IConst 5,
                  IConst 0
              ),
              IConst 3
          ),
          IConst 0
      )
  )),
  IConst 0
);

(* (1 * (35 + (1 * (~3 * 0)))) * ((((~5 + 0) - 0) * 1) - (~5)) = 0 *)
assert( simplify(
  Op2(Mul,
      Op2(Mul,
          IConst 1,
          Op2(Add,
              IConst 35,
              Op2(Mul,
                  IConst 1,
                  Op2(Mul,
                    IConst ~3,
                    IConst 0
                  )
              )
          )
      ),
      Op2(Sub,
          Op2(Mul,
              Op2(Sub,
                  Op2(Add,
                      IConst ~5,
                      IConst 0
                  ),
                  IConst 0
              ),
              IConst 1
          ),
          IConst ~5
      )
  )),
  IConst 0
);
      

(* (35+0) == (1 * 35) -> 35 == 35 *)
assert( simplify(
  Op2(Eq,
      Op2(Add,
          IConst 35,
          IConst 0
      ),
      Op2(Mul,
          IConst 1,
          IConst 35
      )
  )),
  Op2(Eq,IConst 35, IConst 35)
);

(* (1 + 0) ∗ (~323 + 0) = ~323 *)
assert( simplify(
  Op2(Mul,
      Op2(Add,
          IConst 1,
          IConst 0
      ),
      Op2(Add,
          IConst ~323,
          IConst 0
      )
  )),
  IConst ~323
);

(* (1+0)*(9+0) = 9 *)
assert( simplify(
  Op2(Mul, Op2(Add, IConst 1, IConst 0), Op2(Add, IConst 9, IConst 0))),
  IConst 9);

(* (1+0)*((10∨12) +0) -> (10∨12) *)
assert( simplify(
   Op2 (Mul, 
        Op2 (Add, IConst 1, IConst 0), 
        Op2 (Add, 
             Op2 (Or, IConst 10,IConst 12), 
             IConst 0
        )
    )), 
    Op2 (Or, IConst 10, IConst 12)); 
"TESTES CONCLUIDOS!"