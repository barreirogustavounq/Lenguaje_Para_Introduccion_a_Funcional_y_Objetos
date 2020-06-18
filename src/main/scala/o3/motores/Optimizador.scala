package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.Reglamento

class Optimizador() {
  def optimizarOperacion(expresion: Expresion): Expresion = {
    var optimizacion : List[Expresion] = List()
    var resultado = expresion
    Reglamento.reglas.foreach{ r =>
      optimizacion = optimizacion.appended(r.optimizar(expresion))
    }
    optimizacion.foreach{e =>
      if(e != expresion){
        resultado = e
      }
    }
    resultado
  }
  def optimizarPrograma(programa: Programa): Unit ={
    val optimizacion = programa.elementos.map(e => optimizarOperacion(e))
    programa.remplazarOperaciones(optimizacion)
  }
}


