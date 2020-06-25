package o3.expresiones

case class Suma(n1: Expresion, n2: Expresion) extends Operacion
case class Resta(n1: Expresion, n2: Expresion) extends Operacion
case class Multiplicacion(n1: Expresion, n2: Expresion) extends Operacion
case class Division(n1: Expresion, n2: Expresion) extends Operacion
case class Mayor(n1: Expresion, n2: Expresion) extends Operacion
case class Menor(n1: Expresion, n2: Expresion) extends Operacion
case class Igual(n1: Expresion, n2: Expresion) extends Operacion
case class Distinto(n1: Expresion, n2: Expresion) extends Operacion
case class MenorOIgual(n1: Expresion, n2: Expresion) extends Operacion
case class MayorOIgual(n1: Expresion, n2: Expresion) extends Operacion

/**********OPERACION DESCONOCIDA**************/

abstract class Operacion extends Expresion


