package o3.ReglasPredefinidas

import o3.{Programa, Sentencia}
import o3.expresiones._
import o3.gravedad.NivelAdvertencia
import o3.motores.OptimizadorT
import o3.problemas.Problema
import o3.reglamento.Regla

case class ComparacionesSinSentido() extends Regla("Comparación sin sentido", NivelAdvertencia()) with OptimizadorT {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@(_, _: Comparacion) => ps._2 match {
      case Igual(_: Literal, _: Literal) => errorDetectado(ps._2)
      case Distinto(_: Literal, _: Literal) => errorDetectado(ps._2)
      case Mayor(_: Literal, _: Literal) => errorDetectado(ps._2)
      case Menor(_: Literal, _: Literal) => errorDetectado(ps._2)
      case MayorOIgual(_: Literal, _: Literal) => errorDetectado(ps._2)
      case MenorOIgual(_: Literal, _: Literal) => errorDetectado(ps._2)
    }
  }

  def errorDetectado(s: Sentencia): Option[Problema] = {
    Some(new Problema(this, gravedad, s))
  }

  def optimizar(s: Sentencia): Sentencia = {
    s match {
      case s@Igual(_: Literal, _: Literal) => s.ejecutar()
      case s@Distinto(_: Literal, _: Literal) => s.ejecutar()
      case s@Mayor(_: Literal, _: Literal) => s.ejecutar()
      case s@Menor(_: Literal, _: Literal) => s.ejecutar()
      case s@MayorOIgual(_: Literal, _: Literal) => s.ejecutar()
      case s@MenorOIgual(_: Literal, _: Literal) => s.ejecutar()
    }
  }
}
