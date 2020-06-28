package o3.motores

import o3.problemas.ProblemaConRegla
import o3.reglamento.Regla
import o3.{Programa, Sentencia, SentenciaCompuesta, SentenciaSimple}

trait OptimizadorT {
  def optimizar(s: Sentencia): Sentencia

  def optimizar(p: Programa, s: Sentencia): Programa = {
    Programa(reemplazarSentencia(p.sentenciasHijas, optimizar(s)): _*)
  }

  def reemplazarSentencia(ls: List[Sentencia], sentencia: Sentencia): List[Sentencia] = {
    ls.map {
      case s@(ss: SentenciaSimple) => if (ss == s) sentencia else s
      case s@(sc: SentenciaCompuesta) =>
        if (sc == s) sentencia else sc.modificarYRetornar(reemplazarSentencia(sc.sentenciasHijas, sentencia))
    }
  }
}

case class Optimizador(_reglas: Regla with OptimizadorT*) {
  val analizador = Analizador(_reglas.toList)

  def optimizar(programa: Programa): Programa = {
    analizador.analizar(programa).foldLeft(programa: Programa) { (z, problema) => problema match {
      case ProblemaConRegla(r: Regla with OptimizadorT, s) => r.optimizar(z, s)
      case _ => z
    }
    }
  }
}
