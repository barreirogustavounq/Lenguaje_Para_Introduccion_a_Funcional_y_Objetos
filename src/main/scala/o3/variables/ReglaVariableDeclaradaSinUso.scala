package o3.variables

import o3.expresiones.Referencia
import o3.{Programa, Sentencia, SentenciaCompuesta}
import o3.gravedad.NivelAdvertencia
import o3.problemas.Problema
import o3.reglamento.Regla

case class ReglaVariableDeclaradaSinUso() extends Regla("Variable declarada sin uso", NivelAdvertencia()) {

  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case (p, v: Variable) => if (variableSinUso(v.nombre, p.sentenciasHijas)) Some(new Problema(this, gravedad, v)) else None
  }

  def variableSinUso(nombre: String, ls: List[Sentencia]): Boolean = {
    for (s <- ls) {
      s match {
        case r: Referencia => if (r.nombre == nombre) return false
        case s: SentenciaCompuesta => if (!variableSinUso(nombre, s.sentenciasHijas)) return false
        case _ =>
      }
    }
    true
  }
}
