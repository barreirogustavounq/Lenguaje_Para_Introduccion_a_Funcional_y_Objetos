package o3

class Interprete {

  def ejecutarOperacion(operacion: Operacion) = operacion match {
    case Suma(Numero(n1), Numero(n2)) => Numero(n1 + n2)
    case Resta(Numero(n1), Numero(n2)) => Numero(n1 - n2)
    case Multiplicacion(Numero(n1), Numero(n2)) => Numero(n1 * n2)
    case Division(Numero(n1), Numero(n2)) => Numero(n1 / n2)
    case _ => throw new UnsupportedOperationException("No se como interpretar" + operacion)
  }

  def ejecutar(programa: Programa): List[Numero] = {
    var myList = programa.elementos.map(x => ejecutarOperacion(x))
    return myList
    }
}
