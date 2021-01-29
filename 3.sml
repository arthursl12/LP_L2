datatype perimetro = 
  RConst of real 
  | PQuadrado of perimetro 
  | PCirculo of perimetro 
  | PRetangulo of perimetro * perimetro
  | PTriangulo of perimetro * perimetro * perimetro
;

exception LadoNegativoError
fun eval (RConst r) = if (r < 0.0) then raise LadoNegativoError else r
  | eval (PQuadrado(lado)) = eval(lado) + eval(lado) + eval(lado) + eval(lado) 
  | eval (PCirculo(raio)) = 2.0 * eval(raio) * Math.pi
  | eval (PRetangulo(l1,l2)) = eval(l1) + eval(l2) + eval(l1) + eval(l2)
  | eval (PTriangulo(l1,l2,l3)) = eval(l1) + eval(l2) + eval(l3)
;