package o3.motores

import o3.Programa
import o3.expresiones._

class Interprete {

  def ejecutarOperacion(expresion : Expresion, programa: Programa) : Expresion = expresion match {
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
    case v @Variable(_,_) => programa.agregarVariable(v)
    case a @Asignar(ref, valor) => programa.reemplazarReferencia(ref, valor)
    case Suma(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Suma(programa.getValor(n1), Numero(n2)), programa)
    case Suma(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Suma(Numero(n1), programa.getValor(n2)), programa)
    case Suma(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Suma(programa.getValor(n1), programa.getValor(n2)), programa)
    case Resta(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Resta(programa.getValor(n1), Numero(n2)), programa)
    case Resta(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Resta(Numero(n1), programa.getValor(n2)), programa)
    case Resta(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Resta(programa.getValor(n1), programa.getValor(n2)), programa)
    case Multiplicacion(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Multiplicacion(programa.getValor(n1), Numero(n2)), programa)
    case Multiplicacion(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Multiplicacion(Numero(n1), programa.getValor(n2)), programa)
    case Multiplicacion(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Multiplicacion(programa.getValor(n1), programa.getValor(n2)), programa)
    case Division(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Division(programa.getValor(n1), Numero(n2)), programa)
    case Division(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Division(Numero(n1), programa.getValor(n2)), programa)
    case Division(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Division(programa.getValor(n1), programa.getValor(n2)), programa)
    case Mayor(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Mayor(programa.getValor(n1), Numero(n2)), programa)
    case Mayor(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Mayor(Numero(n1), programa.getValor(n2)), programa)
    case Mayor(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Mayor(programa.getValor(n1), programa.getValor(n2)), programa)
    case Menor(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Menor(programa.getValor(n1), Numero(n2)), programa)
    case Menor(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Menor(Numero(n1), programa.getValor(n2)), programa)
    case Menor(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Menor(programa.getValor(n1), programa.getValor(n2)), programa)
    case Igual(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Igual(programa.getValor(n1), Numero(n2)), programa)
    case Igual(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Igual(Numero(n1), programa.getValor(n2)), programa)
    case Igual(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Igual(programa.getValor(n1), programa.getValor(n2)), programa)
    case Distinto(Referencia(n1), Numero(n2)) =>ejecutarOperacion(Distinto(programa.getValor(n1), Numero(n2)), programa)
    case Distinto(Numero(n1), Referencia(n2)) =>ejecutarOperacion(Distinto(Numero(n1), programa.getValor(n2)), programa)
    case Distinto(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(Distinto(programa.getValor(n1), programa.getValor(n2)), programa)
    case MayorOIgual(Referencia(n1), Numero(n2)) =>ejecutarOperacion(MayorOIgual(programa.getValor(n1), Numero(n2)), programa)
    case MayorOIgual(Numero(n1), Referencia(n2)) =>ejecutarOperacion(MayorOIgual(Numero(n1), programa.getValor(n2)), programa)
    case MayorOIgual(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(MayorOIgual(programa.getValor(n1), programa.getValor(n2)), programa)
    case MenorOIgual(Referencia(n1), Numero(n2)) =>ejecutarOperacion(MenorOIgual(programa.getValor(n1), Numero(n2)), programa)
    case MenorOIgual(Numero(n1), Referencia(n2)) =>ejecutarOperacion(MenorOIgual(Numero(n1), programa.getValor(n2)), programa)
    case MenorOIgual(Referencia(n1), Referencia(n2)) =>ejecutarOperacion(MenorOIgual(programa.getValor(n1), programa.getValor(n2)), programa)
    case _ => throw new UnsupportedOperationException("No se como interpretar " + expresion)
  }

  def ejecutar(programa: Programa): List[Expresion] = {
     programa.elementos.map(x => ejecutarOperacion(x, programa))
  }
}
