package o3

import o3.expresiones.{Expresion, Referencia, Variable}

case class Programa(expresiones : List[Expresion]) {
  var variables : List[Variable] = List()
  var elementos : List[Expresion] = expresiones

  def getValor(referencia: String): Expresion ={
    variables.filter(p=> p.nombre == referencia).head
  }
  def reemplazarReferencia(referencia : Referencia,expresion: Expresion): Variable ={
    eliminarVariable(referencia.nombre)
    agregarVariable(Variable(referencia.nombre, expresion))
    Variable(referencia.nombre, expresion)
  }

  def agregarVariable(variable: Variable): Variable ={
    variables = variables.appended(variable)
    variable
  }
  def eliminarVariable(nombreVar: String): Unit ={
    variables = variables.filter(v => v.nombre != nombreVar)
  }
  def remplazarOperaciones(list: List[Expresion]): Unit ={
    elementos = list
  }
}
