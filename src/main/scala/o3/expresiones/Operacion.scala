package o3.expresiones

case class Suma(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Resta(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Multiplicacion(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Division(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Mayor(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Menor(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Igual(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class Distinto(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class MenorOIgual(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)
case class MayorOIgual(n1: Expresion, n2: Expresion) extends Operacion(n1,n2)

/**********OPERACION DESCONOCIDA**************/

class Operacion(n1: Expresion, n2: Expresion) extends Expresion


