package o3

import o3.expresiones.Expresion

import scala.jdk.CollectionConverters._


case class Programa(expresiones : List[Expresion]) {
  var elementos : List[Expresion] = expresiones
  def remplazarOperaciones(list: List[Expresion]): Unit ={
    elementos = list
  }
}

