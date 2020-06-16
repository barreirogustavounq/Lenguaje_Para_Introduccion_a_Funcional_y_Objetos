package o3

class Interprete {

  def ejecutarOperacion(operacion: Operacion) = operacion match {
    case Suma(Numero(n1),Numero(n2)) => Numero(n1 + n2)
    case Resta(Numero(n1),Numero(n2)) => Numero(n1 - n2)
    case Multiplicacion(Numero(n1),Numero(n2)) => Numero(n1 * n2)
    case Division(Numero(n1),Numero(n2)) => Numero(n1 / n2)
  }

  def ejecutar(programa: Programa) = {
    ejecutarOperacion(programa.operacion)
  }
}
