package o3

class Optimizador {

  def optimizarOperacion(operacion: Operacion) = operacion match {
    case  Suma(Numero(n1), Numero(n2)) => optimizarSuma(operacion)
    case  Resta(Numero(n1), Numero(n2)) => optimizarResta(operacion)
    case  Multiplicacion(Numero(n1), Numero(n2)) => optimizarMultiplicacion(operacion)
    case  Division(Numero(n1), Numero(n2)) => optimizarDivision(operacion)
  }

  def optimizarComparacion(comparacion: Comparacion) = comparacion match {
    case  Igual(Numero(n1), Numero(n2)) => optimizarIgualdad(comparacion)
    case  Mayor(Numero(n1), Numero(n2)) => optimizarMayor(comparacion)
    case  Menor(Numero(n1), Numero(n2)) => optimizarMenor(comparacion)
  }

  def optimizarSuma(operacion: Operacion) = operacion match {
    case Suma(Numero(0), Numero(n2)) => Numero(n2)
    case Suma(Numero(n1), Numero(0)) => Numero(n1)
  }

  def optimizarResta(operacion: Operacion) = operacion match {
    case Resta(Numero(n1), Numero(0)) => Numero(n1)
  }

  def optimizarMultiplicacion(operacion: Operacion) = operacion match {
    case Multiplicacion(Numero(n1), Numero(0)) => Numero(0)
    case Multiplicacion(Numero(0), Numero(n2)) => Numero(0)
    case Multiplicacion(Numero(1), Numero(n2)) => Numero(n2)
    case Multiplicacion(Numero(n1), Numero(1)) => Numero(n1)
  }

  def optimizarDivision(operacion: Operacion) = operacion match {
    case Division(Numero(0), Numero(n2)) => Numero(0)
    case Division(Numero(n1), Numero(1)) => Numero(n1)
  }

  def optimizarIgualdad(comparacion: Comparacion) = comparacion match {
    case Igual(Numero(n1), Numero(n2)) if (n1==n2) => Booleano(true)
  }

  def optimizarMayor(comparacion: Comparacion) = comparacion match {
    case Mayor(Numero(n1), Numero(n2)) if (n1>n2) => Booleano(true)
    case Mayor(Numero(n1), Numero(n2)) if (n1<n2) => Booleano(false)
  }

  def optimizarMenor(comparacion: Comparacion) = comparacion match {
    case Menor(Numero(n1), Numero(n2)) if (n1<n2) => Booleano(true)
    case Menor(Numero(n1), Numero(n2)) if (n1>n2) => Booleano(false)
  }

}


