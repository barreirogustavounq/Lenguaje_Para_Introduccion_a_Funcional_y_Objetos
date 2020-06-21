package o3.ReglasPredefinidas

import o3.expresiones._
import o3.gravedad.Advertencia
import o3.problemas.Problema
import o3.reglamento.Regla

object ComparacionesSinSentido extends Regla {
  override def aplicarRegla(expresion: Expresion): Problema = expresion match {
    case Mayor(Numero(n1), Numero(n2)) if n1 > n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case Mayor(Numero(n1), Numero(n2)) if !(n1 > n2) => Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
    case Menor(Numero(n1), Numero(n2)) if n1 < n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case Menor(Numero(n1), Numero(n2)) if !(n1 < n2) => Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
    case Igual(Numero(n1), Numero(n2)) if n1 == n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case Igual(Numero(n1), Numero(n2)) if !(n1 == n2)=> Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
    case Distinto(Numero(n1), Numero(n2)) if n1 != n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case Distinto(Numero(n1), Numero(n2)) if n1 == n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
    case MayorOIgual(Numero(n1), Numero(n2)) if n1 >= n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case MayorOIgual(Numero(n1), Numero(n2)) if !(n1 >= n2)=> Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
    case MenorOIgual(Numero(n1), Numero(n2)) if n1 <= n2 => Problema(Advertencia, "comparación sin sentido: siempre retorna true", expresion)
    case MenorOIgual(Numero(n1), Numero(n2)) if !(n1 <= n2)=> Problema(Advertencia, "comparación sin sentido: siempre retorna false", expresion)
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
