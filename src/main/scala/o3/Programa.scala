package o3

import o3.expresiones.{Expresion, Variable}

import scala.collection.mutable

case class Programa(expresiones : List[Expresion]) {
  var variables : mutable.HashMap[String, Expresion] = mutable.HashMap()

  var elementos : List[Expresion] = expresiones
  def remplazarOperaciones(list: List[Expresion]): Unit ={
    elementos = list
  }
}