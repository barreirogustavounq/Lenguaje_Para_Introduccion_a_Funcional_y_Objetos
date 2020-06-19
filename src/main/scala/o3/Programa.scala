package o3

import o3.expresiones.{Expresion, Variable}

case class Programa(expresiones : List[Expresion]) {
  var variables : List[Variable] = List()

  var elementos : List[Expresion] = expresiones
  def remplazarOperaciones(list: List[Expresion]): Unit ={
    elementos = list
  }
}