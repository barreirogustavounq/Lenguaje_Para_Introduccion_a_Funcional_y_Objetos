package o3.ReglasPredefinidas

import o3.expresiones.{Division, Expresion, Numero}
import o3.gravedad.Error
import o3.problemas.Problema
import o3.reglamento.Regla

/********************* REGLAS DEFINIDAS EN LA CONSIGNA ***********************/
object DividirPorCero extends Regla{
  override def aplicarRegla(expresion: Expresion): Problema = expresion match {
    case Division(Numero(_), Numero(0)) => Problema(Error, "No se puede dividir por cero", expresion)
    super.aplicarRegla(expresion)
  }
  override def optimizar(expresion: Expresion): Expresion = {
    super.optimizar(expresion)
  }
}
