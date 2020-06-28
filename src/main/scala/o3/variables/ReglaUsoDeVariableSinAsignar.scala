package o3.variables

import o3.expresiones.{Referencia, Valor}
import o3.{Programa, Sentencia, SentenciaCompuesta}
import o3.gravedad.NivelAdvertencia
import o3.problemas.Problema
import o3.reglamento.Regla

case class ReglaUsoDeVariableSinAsignar() extends Regla("Uso de variable sin asignar", NivelAdvertencia()) {

  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case (p, r: Referencia) => if (referenciaAVariableSinAsginar(r.nombre, p.sentenciasHijas)) Some(new Problema(this, gravedad, r)) else None
  }

  def referenciaAVariableSinAsginar(nombre: String, ls: List[Sentencia]): Boolean = {
    for (s <- ls) {
      s match {
        case Referencia(n) => if (n == nombre) return true
        case Variable(n, v:Valor) => if (n == nombre && v!=null) return false
        case Asignar(Referencia(n), _:Valor) => if (n == nombre) return false
        case s: SentenciaCompuesta => if (referenciaAVariableSinAsginar(nombre, s.sentenciasHijas)) return true
        case _ =>
      }
    }
    false
  }
}
