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