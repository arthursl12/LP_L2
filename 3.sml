datatype perimetro = 
  RConst of real 
  | PQuadrado of perimetro 
  | PCirculo of perimetro 
  | PRetangulo of perimetro * perimetro
  | PTriangulo of perimetro * perimetro * perimetro
;

fun eval (RConst r) = r
  | eval (PQuadrado(lado)) = eval(lado) + eval(lado) + eval(lado) + eval(lado) 
  | eval (PCirculo(raio)) = 2.0 * eval(raio) * Math.pi
  | eval (PRetangulo(l1,l2)) = eval(l1) + eval(l2) + eval(l1) + eval(l2)
  | eval (PTriangulo(l1,l2,l3)) = eval(l1) + eval(l2) + eval(l3)
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
assertR(eval(PQuadrado(RConst 1.5)), 6.0);
assertR(eval(PQuadrado(RConst 0.0)), 0.0);

(* Cículo *)
assertR(eval(PCirculo(RConst 1.0)), 6.283185);
assertR(eval(PCirculo(RConst 0.0)), 0.0);
assertR(eval(PCirculo(RConst 3.5)), 21.99114858);

(* Retângulo *)
assertR(eval(PRetangulo(RConst 12.0, RConst 3.5)), 31.0);
assertR(eval(PRetangulo(RConst 12.0, RConst 0.0)), 24.0);
assertR(eval(PRetangulo(RConst 0.0, RConst 0.0)), 0.0);


(* Triângulo *)
assertR(eval(PTriangulo(RConst 12.0, RConst 3.5, RConst 5.5)), 21.0);
assertR(eval(PTriangulo(RConst 0.0, RConst 0.0, RConst 5.3)), 5.3);
assertR(eval(PTriangulo(RConst 0.0, RConst 0.0, RConst 0.0)), 0.0);
"TESTES CONCLUIDOS COM SUCESSO!"