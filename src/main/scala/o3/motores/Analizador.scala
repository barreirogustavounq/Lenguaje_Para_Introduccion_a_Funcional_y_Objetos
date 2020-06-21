package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.Reglamento
import o3.problemas.Problema

class Analizador {

  def analizarOperacion(expresion: Expresion) : Problema = {
    val r = Reglamento.relgaPara(expresion)
    r.aplicarRegla(expresion)
  }

  def analizar(programa: Programa) : List[Problema] = {
    programa.elementos.map(op => analizarOperacion(op))
  }
}













