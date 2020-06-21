package o3.ReglasPredefinidas

import o3.expresiones._
import o3.gravedad.Advertencia
import o3.problemas.Problema
import o3.reglamento.Regla

object ComparacionesSinSentido extends Regla {
  override def aplicarRegla(expresion: Expresion): Problema = expresion match {
    case e @ Mayor(Numero(n1), Numero(n2)) if n1 > n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ Mayor(Numero(n1), Numero(n2)) if !(n1 > n2) => Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    case e @ Menor(Numero(n1), Numero(n2)) if n1 < n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ Menor(Numero(n1), Numero(n2)) if !(n1 < n2) => Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    case e @ Igual(Numero(n1), Numero(n2)) if n1 == n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ Igual(Numero(n1), Numero(n2)) if !(n1 == n2)=> Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    case e @ Distinto(Numero(n1), Numero(n2)) if n1 != n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ Distinto(Numero(n1), Numero(n2)) if n1 == n2 => Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    case e @ MayorOIgual(Numero(n1), Numero(n2)) if n1 >= n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ MayorOIgual(Numero(n1), Numero(n2)) if !(n1 >= n2)=> Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    case e @ MenorOIgual(Numero(n1), Numero(n2)) if n1 <= n2 => Problema(Advertencia, "operacion redundante: siempre retorna true", e)
    case e @ MenorOIgual(Numero(n1), Numero(n2)) if !(n1 <= n2)=> Problema(Advertencia, "operacion redundante: siempre retorna false", e)
    super.aplicarRegla(expresion)
  }

  override def optimizar(expresion: Expresion): Expresion = expresion match {
    case Mayor(Numero(n1), Numero(n2))if n1 > n2 => Booleano(true)
    case Mayor(Numero(n1), Numero(n2))if !(n1 > n2) => Booleano(false)
    case Menor(Numero(n1), Numero(n2))if n1 < n2 => Booleano(true)
    case Menor(Numero(n1), Numero(n2))if !(n1 < n2)=> Booleano(false)
    case Igual(Numero(n1), Numero(n2))if n1 == n2 => Booleano(true)
    case Igual(Numero(n1), Numero(n2))if !(n1 == n2)=> Booleano(false)
    case Distinto(Numero(n1), Numero(n2))if n1 != n2 => Booleano(true)
    case Distinto(Numero(n1), Numero(n2))if n1 == n2 =>Booleano(false)
    case MayorOIgual(Numero(n1), Numero(n2))if n1 >= n2 => Booleano(true)
    case MayorOIgual(Numero(n1), Numero(n2))if !(n1 >= n2)=>Booleano(false)
    case MenorOIgual(Numero(n1), Numero(n2))if n1 <= n2 => Booleano(true)
    case MenorOIgual(Numero(n1), Numero(n2))if !(n1 <= n2)=>Booleano(false)
    super.optimizar(expresion)
  }
}
