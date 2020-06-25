package o3.ReglasPredefinidas

import o3.expresiones._
import o3.gravedad.{Advertencia, Ok}
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
    case _ => Problema(Ok, "no hay problemas en la operacion", expresion)
  }

  override def optimizar(expresion: Expresion): Expresion = expresion match {
    case Mayor(Numero(n1), Numero(n2)) => if (n1 > n2) True else False
    case Mayor(Numero(n1), Numero(n2)) => if (!(n1 > n2)) True else False
    case Menor(Numero(n1), Numero(n2)) => if (n1 < n2) True else False
    case Menor(Numero(n1), Numero(n2)) => if (!(n1 < n2)) True else False
    case Igual(Numero(n1), Numero(n2)) => if (n1 == n2) True else False
    case Igual(Numero(n1), Numero(n2)) => if (!(n1 == n2)) True else False
    case Distinto(Numero(n1), Numero(n2)) => if (n1 != n2) True else False
    case Distinto(Numero(n1), Numero(n2)) => if (n1 == n2) True else False
    case MayorOIgual(Numero(n1), Numero(n2)) => if (n1 >= n2) True else False
    case MayorOIgual(Numero(n1), Numero(n2)) => if (!(n1 >= n2)) True else False
    case MenorOIgual(Numero(n1), Numero(n2)) => if (n1 <= n2) True else False
    case MenorOIgual(Numero(n1), Numero(n2)) => if (!(n1 <= n2)) True else False
    case _ => expresion
  }
}
