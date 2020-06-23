package o3.motores

import o3.Programa
import o3.expresiones.Expresion
import o3.gravedad.{Advertencia, Error}
import o3.reglamento.Regla
import o3.problemas.Problema

class Analizador {

  def analizarOperacion(expresion: Expresion, reglas : List[Regla]) : List[Problema] = {
    var respuesta : List[Problema] = List()
    reglas.foreach(r => r.aplicarRegla(expresion) match {
      case Problema(Advertencia,_,_) => respuesta = respuesta :+ r.aplicarRegla(expresion)
      case Problema(Error,_,_) => respuesta = respuesta :+ r.aplicarRegla(expresion)
      case _ => None
    })
    respuesta
  }

  def analizar(programa: Programa, reglas : List[Regla]) : List[List[Problema]] = {
    programa.elementos.map(op => analizarOperacion(op, reglas))
  }
}













