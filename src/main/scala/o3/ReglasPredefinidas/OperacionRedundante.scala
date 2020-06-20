package o3.ReglasPredefinidas

import o3.expresiones._
import o3.gravedad.{Advertencia, Ok}
import o3.reglamento.Regla
import o3.respuesta
import o3.respuesta.Respuesta

object OperacionRedundante extends Regla {

  override def aplicarRegla(expresion: Expresion): Respuesta = expresion match {
    case Suma(Numero(0), Numero(n)) => respuesta.Respuesta(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", "Suma")
    case Suma(Numero(n), Numero(0)) => respuesta.Respuesta(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", "Suma")
    case Resta(Numero(n), Numero(0)) => respuesta.Respuesta(Advertencia, "operacion redundante: restar 0 retorna el mismo numero", "Resta")
    case Multiplicacion(Numero(1), Numero(n)) => respuesta.Respuesta(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero ", "Multiplicacion")
    case Multiplicacion(Numero(n), Numero(1)) => respuesta.Respuesta(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", "Multiplicacion")
    case Division(Numero(n), Numero(1)) => respuesta.Respuesta(Advertencia, "operacion redundante: dividir por 1 retorna el mismo numero", "Division")
    case Mayor(Numero(n1), Numero(n2)) if (n1 > n2) => respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion Mayor")
    case Mayor(Numero(n1), Numero(n2)) if (!(n1 > n2)) => respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion Mayor")
    case Menor(Numero(n1), Numero(n2)) if (n1 < n2) => respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion Menor")
    case Menor(Numero(n1), Numero(n2)) if !(n1 < n2) => respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion Menor")
    case Igual(Numero(n1), Numero(n2)) if (n1 == n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion Igual")
    case Igual(Numero(n1), Numero(n2)) if !(n1 == n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion Igual")
    case Distinto(Numero(n1), Numero(n2)) if (n1 != n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion Distinto")
    case Distinto(Numero(n1), Numero(n2)) if n1 == n2=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion Distinto")
    case MayorOIgual(Numero(n1), Numero(n2)) if (n1 >= n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion MayorIgual")
    case MayorOIgual(Numero(n1), Numero(n2)) if !(n1 >= n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion MayorIgua")
    case MenorOIgual(Numero(n1), Numero(n2)) if (n1 <= n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna true", "Comparacion MenorIgual")
    case MenorOIgual(Numero(n1), Numero(n2)) if !(n1 <= n2)=> respuesta.Respuesta(Advertencia, "operacion redundante: siempre retorna false", "Comparacion MenorIgual")
    case _ => respuesta.Respuesta(Ok, "no hay problemas en la operacion", expresion.getClass.getSimpleName)
  }

  override def optimizar(expresion: Expresion): Expresion = expresion match {
    case Suma(Numero(0), Numero(n))=> Numero(n)
    case Suma(Numero(n), Numero(0))=> Numero(n)
    case Resta(Numero(n), Numero(0))=> Numero(n)
    case Resta(Numero(0), Numero(n))=> Numero(-n)
    case Multiplicacion(Numero(1), Numero(n))=> Numero(n)
    case Multiplicacion(Numero(n), Numero(1))=> Numero(n)
    case Division(Numero(n), Numero(1))=> Numero(n)
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

    case _ => expresion
  }
}
