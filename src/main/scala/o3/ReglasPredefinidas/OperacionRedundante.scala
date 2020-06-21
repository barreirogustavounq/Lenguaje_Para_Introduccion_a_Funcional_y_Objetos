package o3.ReglasPredefinidas

import o3.expresiones._
import o3.gravedad.{Advertencia, Ok}
import o3.problemas.Problema
import o3.reglamento.Regla

object OperacionRedundante extends Regla {

  override def aplicarRegla(expresion: Expresion): Problema = expresion match {
    case Suma(Numero(0), Numero(_)) => Problema(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", expresion)
    case Suma(Numero(_), Numero(0)) => Problema(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", expresion)
    case Resta(Numero(_), Numero(0)) => Problema(Advertencia, "operacion redundante: restar 0 retorna el mismo numero", expresion)
    case Multiplicacion(Numero(1), Numero(_)) => Problema(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", expresion)
    case Multiplicacion(Numero(_), Numero(1)) => Problema(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", expresion)
    case Division(Numero(n), Numero(1)) => Problema(Advertencia, "operacion redundante: dividir por 1 retorna el mismo numero", Division(Numero(n), Numero(1)))
    case _ => Problema(Ok, "no hay problemas en la operacion", expresion)
  }

  override def optimizar(expresion: Expresion): Expresion = expresion match {
    case Suma(Numero(0), Numero(n))=> Numero(n)
    case Suma(Numero(n), Numero(0))=> Numero(n)
    case Resta(Numero(n), Numero(0))=> Numero(n)
    case Resta(Numero(0), Numero(n))=> Numero(-n)
    case Multiplicacion(Numero(1), Numero(n))=> Numero(n)
    case Multiplicacion(Numero(n), Numero(1))=> Numero(n)
    case Division(Numero(n), Numero(1))=> Numero(n)
    case _ => expresion
  }
}
