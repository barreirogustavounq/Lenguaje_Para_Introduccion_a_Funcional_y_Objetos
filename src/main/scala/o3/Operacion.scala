package o3

case class Suma(n1: Numero, n2: Numero) extends Operacion(n1,n2)
case class Resta(n1: Numero, n2: Numero) extends Operacion(n1,n2)
case class Multiplicacion(n1: Numero, n2: Numero) extends Operacion(n1,n2)
case class Division(n1: Numero, n2: Numero) extends Operacion(n1,n2)

class Operacion(n1: Numero, n2: Numero)
