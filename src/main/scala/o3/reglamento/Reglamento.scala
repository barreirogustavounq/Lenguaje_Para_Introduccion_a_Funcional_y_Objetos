package o3.reglamento

import o3.ReglasPredefinidas._
import o3.expresiones._

/****************REGLAMENTO******************************/
object  Reglamento {
  def relgaPara(expresion: Expresion) = expresion match {
    case Division(Numero(_),Numero(0)) => DividicionPorCero
    case Division(Numero(_),Numero(_)) => OperacionRedundante
    case Suma(Numero(_),Numero(_)) => OperacionRedundante
    case Resta(Numero(_),Numero(_)) => OperacionRedundante
    case Multiplicacion(Numero(_),Numero(_)) => OperacionRedundante
    case Mayor(Numero(_),Numero(_)) => ComparacionesSinSentido
    case Menor(Numero(_),Numero(_)) => ComparacionesSinSentido
    case MayorOIgual(Numero(_),Numero(_)) => ComparacionesSinSentido
    case MenorOIgual(Numero(_),Numero(_)) => ComparacionesSinSentido
    case Igual(Numero(_),Numero(_)) => ComparacionesSinSentido
    case Distinto(Numero(_),Numero(_)) => ComparacionesSinSentido
  }
}
