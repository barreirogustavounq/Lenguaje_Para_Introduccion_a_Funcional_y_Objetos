package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.Reglamento
import o3.problemas.Problema

class Analizador {


  def analizarOperacion(expresion: Expresion) : List[Problema] = {
    val problemas : List[Problema] = List()
    Reglamento.reglas.foreach { r =>
      problemas :+ r.aplicarRegla(expresion)
    }
    problemas
  }

  def analizar(programa: Programa) : List[List[Problema]] = {
    programa.elementos.map(op => analizarOperacion(op))
  }
}













