package o3.motores

import o3.Programa
import o3.expresiones._

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
//    case v @Variable(_,_) => v.valor
//    case Suma(v1 @Variable(_,_), v2@Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Resta(v1 @Variable(_,_), v2@Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Multiplicacion(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Division(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Mayor(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Menor(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Igual(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case Distinto(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case MayorOIgual(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
//    case MenorOIgual(v1 @Variable(_,_), v2 @Variable(_,_)) =>ejecutarOperacion(MenorOIgual(v1.valor, v2.valor))
    case _ => throw new UnsupportedOperationException("No se como interpretar " + expresion)
  }

  def ejecutar(programa: Programa): List[Expresion] = {
     programa.elementos.map(x => ejecutarOperacion(x))
  }
}
