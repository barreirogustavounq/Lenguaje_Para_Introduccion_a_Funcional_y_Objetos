package o3.reglamento

import o3.expresiones.Expresion
import o3.respuesta.Respuesta

/**************************MOTOR DE REGLAS*************************************/
trait Regla {
  def aplicarRegla(expresion: Expresion): Respuesta

  def optimizar(expresion: Expresion) : Expresion
}
