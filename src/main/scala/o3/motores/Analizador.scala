package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.reglamento.Reglamento
import o3.respuesta.Respuesta

class Analizador {


  def analizarOperacion(expresion: Expresion) : List[Respuesta] = {
    var respuestas : List[Respuesta] = List()
    Reglamento.reglas.foreach { r =>
      respuestas = respuestas.appended(r.aplicarRegla(expresion))
    }
    respuestas
  }

  def analizar(programa: Programa) : List[List[Respuesta]] = {
    val myList = programa.elementos.map(op => analizarOperacion(op))
    println(myList)
    myList
  }
}













