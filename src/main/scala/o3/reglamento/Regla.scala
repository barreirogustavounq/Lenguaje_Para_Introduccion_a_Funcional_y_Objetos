package o3.reglamento

import o3.expresiones.Expresion
import o3.problemas.Problema

/**************************MOTOR DE REGLAS*************************************/
trait Regla {
  def aplicarRegla(expresion: Expresion): Problema

  def optimizar(expresion: Expresion): Expresion
}
