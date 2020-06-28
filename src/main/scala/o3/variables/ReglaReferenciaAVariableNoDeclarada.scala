package o3.variables

import o3.expresiones.Referencia
import o3.gravedad.NivelAdvertencia
import o3.{Programa, Sentencia, SentenciaCompuesta}
import o3.problemas.Problema
import o3.reglamento.Regla

case class ReglaReferenciaAVariableNoDeclarada() extends Regla("Referencia a variable no declarada", NivelAdvertencia()) {

  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case (p, r: Referencia) => if (referenciaNoDeclarada(r.nombre, p.sentenciasHijas)) Some(new Problema(this, gravedad, r)) else None
  }

  def referenciaNoDeclarada(nombre: String, ls: List[Sentencia]): Boolean = {
    for (s <- ls) {
      s match {
        case Referencia(n) => if (n == nombre) return true
        case Variable(n, _) => if (n == nombre) return false
        case s: SentenciaCompuesta => if (referenciaNoDeclarada(nombre, s.sentenciasHijas)) return true
        case _ =>
      }
    }
    false
  }
}
