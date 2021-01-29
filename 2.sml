datatype area = 
  RConst of real 
  | AQuadrado of area 
  | ACirculo of area 
  | ARetangulo of area * area
;

exception LadoNegativoError
fun eval (RConst r) = if (r < 0.0) then raise LadoNegativoError else r
  | eval (AQuadrado(lado)) = eval(lado) * eval(lado) 
  | eval (ACirculo(raio)) = eval(raio) * eval(raio) * Math.pi
  | eval (ARetangulo(l1,l2)) = eval(l1) * eval(l2)
;