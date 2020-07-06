package o3

import o3.expresiones.Valor
import o3.motores.Contexto

abstract class Sentencia {
  def ejecutar(contexto: Contexto): Valor
}

abstract class SentenciaSimple extends Sentencia {}

abstract class SentenciaCompuesta(sentencias: Sentencia*) extends Sentencia {
  var sentenciasHijas: List[Sentencia] = sentencias.toList

  def modificarYRetornar(ls: List[Sentencia]): SentenciaCompuesta = {
    sentenciasHijas = ls
    this
  }
}
