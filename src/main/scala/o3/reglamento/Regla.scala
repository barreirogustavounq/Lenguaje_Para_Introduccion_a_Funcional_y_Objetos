package o3.reglamento

import o3.expresiones.Expresion
import o3.gravedad.Ok
import o3.problemas.Problema

/**************************MOTOR DE REGLAS*************************************/
trait Regla {
  def aplicarRegla(expresion: Expresion): Problema = expresion match {
    case _ => Problema(Ok, "no hay problemas en la operacion", expresion)
  }

  def optimizar(expresion: Expresion) : Expresion = expresion match {
    case _ => expresion
  }
}
