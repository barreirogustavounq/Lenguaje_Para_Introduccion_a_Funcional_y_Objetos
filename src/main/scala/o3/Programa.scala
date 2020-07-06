package o3

import o3.expresiones._
import o3.motores.Contexto

case class Programa(sentencias: Sentencia*) extends SentenciaCompuesta(sentencias: _*) {
  override def ejecutar(contexto: Contexto = Contexto()): Valor = {
    sentenciasHijas.foldLeft(Nulo(): Valor) {
      (_, s) => s.ejecutar(contexto)
    }
  }
}
