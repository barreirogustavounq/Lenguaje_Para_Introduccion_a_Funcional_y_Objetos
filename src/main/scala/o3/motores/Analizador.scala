package o3.motores

import o3.problemas.Problema
import o3.reglamento.Regla
import o3.{Programa, Sentencia, SentenciaCompuesta, SentenciaSimple}

case class Analizador(var reglas: List[Regla]) {

  def analizar(programa: Programa): List[Problema] = {
    reglas.foldLeft(List[Problema]()) { (z, r) =>
      z ++ analizarSentencia(programa, programa.asInstanceOf[SentenciaCompuesta], r)
    }
  }

  def analizarSentencia(p: Programa, s: Sentencia, r: Regla): List[Problema] = {
    s match {
      case s: SentenciaCompuesta =>
        r.analizar(p, s).toList ++ s.sentenciasHijas.foldLeft(List[Problema]()) { (z, s) => z ++ analizarSentencia(p, s, r) }
      case s: SentenciaSimple => r.analizar(p, s).toList
    }
  }
}













