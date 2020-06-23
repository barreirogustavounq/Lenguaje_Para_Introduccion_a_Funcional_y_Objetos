package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.{Regla, Reglamento}

class Optimizador() {
  def optimizarOperacion(expresion: Expresion, reglas : List[Regla]): Expresion = {
    var optimizaciones : List[Expresion] = List()
    reglas.foreach(regla => optimizaciones = optimizaciones.appended(regla.optimizar(expresion)))
    optimizaciones.last
  }
  def optimizarPrograma(programa: Programa, reglas: List[Regla]): Unit ={
    val optimizacion = programa.elementos.map(e => optimizarOperacion(e, reglas))
    programa.remplazarOperaciones(optimizacion)
  }
}


