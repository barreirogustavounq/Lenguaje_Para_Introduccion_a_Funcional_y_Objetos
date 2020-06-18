package o3.ReglasPredefinidas

import o3.expresiones.{Division, Expresion, Numero}
import o3.reglamento.Regla
import o3.gravedad.{Error, Ok}
import o3.respuesta
import o3.respuesta.Respuesta

/********************* REGLAS DEFINIDAS EN LA CONSIGNA ***********************/
object DividirPorCero extends Regla{
  override def aplicarRegla(expresion: Expresion): Respuesta = expresion match {
    case Division(Numero(_), Numero(0)) => respuesta.Respuesta(Error, "No se puede dividir por cero", expresion.getClass.getSimpleName)
    case _ => respuesta.Respuesta(Ok, "no hay problemas en la operacion", expresion.getClass.getSimpleName)
  }
  override def optimizar(expresion: Expresion): Expresion = expresion match {
    case Division(Numero(_), Numero(0))=> throw new Exception("No es posible dividir por cero")
    case _ => expresion
  }
}
