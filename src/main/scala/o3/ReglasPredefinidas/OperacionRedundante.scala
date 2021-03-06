package o3.ReglasPredefinidas

import o3.expresiones.{Division, Multiplicacion, Numero, Resta, Suma}
import o3.gravedad.NivelAdvertencia
import o3.motores.OptimizadorT
import o3.problemas.Problema
import o3.{Programa, Sentencia}
import o3.reglamento.Regla

case class ReglaNoSumarCero() extends Regla("No sumar cero", NivelAdvertencia()) with OptimizadorT {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@((_, Suma(Numero(0), _)) | (_, Suma(_, Numero(0)))) => Some(new Problema(this, gravedad, ps._2))
  }

  override def optimizar(s: Sentencia): Sentencia = {
    s match {
      case Suma(Numero(0), a) => a
      case Suma(a, Numero(0)) => a
    }
  }
}

case class ReglaNoRestarCero() extends Regla("No restar cero", NivelAdvertencia()) with OptimizadorT {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@(_, Resta(_, Numero(0))) => Some(new Problema(this, gravedad, ps._2))
  }

  override def optimizar(s: Sentencia): Sentencia = {
    s match {
      case Resta(a, Numero(0)) => a
    }
  }
}

case class ReglaNoMultiplicarPorUno() extends Regla("No multiplicar por 1", NivelAdvertencia()) with OptimizadorT {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@((_, Multiplicacion(_, Numero(1))) | (_, Multiplicacion(Numero(1), _))) => Some(new Problema(this, gravedad, ps._2))
  }

  override def optimizar(s: Sentencia): Sentencia = {
    s match {
      case Multiplicacion(Numero(1), a) => a
      case Multiplicacion(a, Numero(1)) => a
    }
  }
}

case class ReglaNoDividirPorUno() extends Regla("No dividir por 1", NivelAdvertencia()) with OptimizadorT {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]] = {
    case ps@((_, Division(_, Numero(1)))) => Some(new Problema(this, gravedad, ps._2))
  }

  def optimizar(s: Sentencia): Sentencia = {
    s match {
      case Division(a, Numero(1)) => a
      case Division(Numero(1), a) => a
    }
  }
}


