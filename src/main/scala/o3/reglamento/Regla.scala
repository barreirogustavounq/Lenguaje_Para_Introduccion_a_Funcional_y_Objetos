package o3.reglamento

import o3.gravedad.Gravedad
import o3.problemas.Problema
import o3.{Programa, Sentencia}

/**************************MOTOR DE REGLAS*************************************/
abstract class Regla(val mensaje: String, val gravedad: Gravedad) {
  val fn: PartialFunction[(Programa, Sentencia), Option[Problema]]

  def analizar(programa: Programa, sentencia: Sentencia): Option[Problema] = {
    if (fn.isDefinedAt((programa, sentencia)))
      fn.apply((programa, sentencia))
    else None
  }
}
