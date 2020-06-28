package o3.motores

import o3.Programa
import o3.expresiones.Valor

case class Ejecutador(var contexto: Contexto = Contexto()) {
  def ejecutar(p: Programa): Valor = {
    p.ejecutar(contexto)
  }
}
