package o3

class Interprete {

  def ejecutarOperacion(expresion : Expresion) : Expresion = expresion match {
    case Suma(Numero(n1), Numero(n2)) => Numero(n1 + n2)
    case Resta(Numero(n1), Numero(n2)) => Numero(n1 - n2)
    case Multiplicacion(Numero(n1), Numero(n2)) => Numero(n1 * n2)
    case Division(Numero(n1), Numero(n2)) => Numero(n1 / n2)
    case Numero(n) => Numero(n)
    case Mayor(Numero(n1), Numero(n2)) => Booleano(n1 > n2)
    case Menor(Numero(n1), Numero(n2)) => Booleano(n1 < n2)
    case Igual(Numero(n1), Numero(n2)) => Booleano(n1 == n2)
    case Distinto(Numero(n1), Numero(n2)) => Booleano(n1 != n2)
    case MayorOIgual(Numero(n1), Numero(n2)) => Booleano(n1 >= n2)
    case MenorOIgual(Numero(n1), Numero(n2)) => Booleano(n1 <= n2)
    case _ => throw new UnsupportedOperationException("No se como interpretar" + expresion)
  }

  def ejecutar(programa: Programa): List[Expresion] = {
    val myList = programa.elementos.map(x => ejecutarOperacion(x))
    return myList
    }
}
