package o3.ReglasPredefinidas

import o3.expresiones.{Division, Numero}
import o3.gravedad.NivelError
import o3.problemas.Problema
import o3.reglamento.Regla
import o3.{Programa, Sentencia}

case class DividicionPorCero() extends Regla("No dividir por cero", NivelError()) {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@(_, Division(_, Numero(0))) => Some(new Problema(this, gravedad, ps._2))
  }
}
