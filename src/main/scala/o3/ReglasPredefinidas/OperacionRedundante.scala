package o3.ReglasPredefinidas

import o3.expresiones.{Division, Multiplicacion, Numero, Resta, Suma}
import o3.gravedad.NivelAdvertencia
import o3.motores.OptimizadorT
import o3.problemas.Problema
import o3.{Programa, Sentencia}
import o3.reglamento.Regla

//object OperacionRedundante extends Regla {
//  override def aplicarRegla(expresion: Expresion): Problema = expresion match {
//    case Suma(Numero(0), Numero(_)) | Suma(Numero(_), Numero(0)) => Problema(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", expresion)
//    case Resta(Numero(_), Numero(0)) => Problema(Advertencia, "operacion redundante: restar 0 retorna el mismo numero", expresion)
//    case Multiplicacion(Numero(1), Numero(_)) | Multiplicacion(Numero(_), Numero(1)) => Problema(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", expresion)
//    case Division(Numero(n), Numero(1)) => Problema(Advertencia, "operacion redundante: dividir por 1 retorna el mismo numero", Division(Numero(n), Numero(1)))
//    case _ => Problema(Ok, "no hay problemas en la operacion", expresion)
//  }
//
//  override def optimizar(expresion: Expresion): Expresion = expresion match {
//    case Suma(Numero(0), Numero(n))=> Numero(n)
//    case Suma(Numero(n), Numero(0))=> Numero(n)
//    case Resta(Numero(n), Numero(0))=> Numero(n)
//    case Resta(Numero(0), Numero(n))=> Numero(-n)
//    case Multiplicacion(Numero(1), Numero(n))=> Numero(n)
//    case Multiplicacion(Numero(n), Numero(1))=> Numero(n)
//    case Division(Numero(n), Numero(1))=> Numero(n)
//    case _ => expresion
//  }
//}
//}

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


