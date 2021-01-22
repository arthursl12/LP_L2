datatype area = 
  RConst of real 
  | AQuadrado of area 
  | ACirculo of area 
  | ARetangulo of area * area
;

fun eval (RConst r) = r
  | eval (AQuadrado(lado)) = eval(lado) * eval(lado) 
  | eval (ACirculo(raio)) = eval(raio) * eval(raio) * Math.pi
  | eval (ARetangulo(l1,l2)) = eval(l1) * eval(l2)
;

exception AssertionError
fun assert (e1: ''a, expected: ''a) =
      if e1 <> expected then 
        raise AssertionError
      else
        true
;
fun assertR (e1,expected) =
   if (abs(e1 - expected) > 0.00001) then 
      raise AssertionError
   else
      true
;

(* Quadrado *)
assertR(eval(AQuadrado(RConst 1.0)), 1.0);
assertR(eval(AQuadrado(RConst 0.0)), 0.0);

(* Cículo *)
assertR(eval(ACirculo(RConst 1.0)), 3.14159265);
assertR(eval(ACirculo(RConst 0.0)), 0.0);
assertR(eval(ACirculo(RConst 3.5)), 38.48451);

(* Retângulo *)
assertR(eval(ARetangulo(RConst 12.0, RConst 3.5)), 42.0);
assertR(eval(ARetangulo(RConst 12.0, RConst 0.0)), 0.0);
"TESTES CONCLUIDOS COM SUCESSO!"