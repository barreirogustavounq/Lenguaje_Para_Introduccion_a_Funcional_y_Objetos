package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.Reglamento

class Optimizador() {
  def optimizarOperacion(expresion: Expresion): Expresion = {
    val r = Reglamento.relgaPara(expresion)
    r.optimizar(expresion)
  }
  def optimizarPrograma(programa: Programa): Unit ={
    val optimizacion = programa.elementos.map(e => optimizarOperacion(e))
    programa.remplazarOperaciones(optimizacion)
  }
}


