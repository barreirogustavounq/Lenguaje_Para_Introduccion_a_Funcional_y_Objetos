package o3.expresiones

import scala.collection.mutable

class Variable(val nombre : String, var valor : Expresion) extends Expresion {

}

object Variable {
  def apply(nombre: String, valor: Expresion): Variable = {
    val variable = new Variable(nombre, valor)
    Referencia.agregarVariable(variable)
    variable
  }

  def unapply(arg: Variable): Option[(String, Expresion)] = Some(arg.nombre, arg.valor)
}

case class Asignar(variable: Variable, valor : Expresion) extends Expresion

object Referencia {
  var variables : mutable.HashMap[String, Expresion] = mutable.HashMap()

  def clear(): Unit = variables.clear()

  def agregarVariable(variable : Variable): Unit = {
    variables.get(variable.nombre) match {
      case None => variables(variable.nombre) = variable.valor
      case _ => throw new ExcepcionVariableExistente
    }
  }

  def encontrarReferenciaCon(nombre: String): Expresion = {
    var variable: Expresion = null
    try {
      variable = variables(nombre)
    } catch {
      case e : NoSuchElementException => throw new ExcepcionVariableInexistente
    }
    variable
  }

  def apply(nombre: String): Expresion = {
    encontrarReferenciaCon(nombre)
  }
}

class ExcepcionVariableInexistente extends Exception
class ExcepcionVariableExistente extends Exception

//case class Referencia(nombre : String)  extends Expresion {}

//class AnalizadorVariable {
//}
//
//object Chequeo {
//
//  def asignarReferencia(asignar: Asignar, programa: Programa): Unit = asignar match {
//    case Asignar(ref : Referencia, exp : Expresion) => reemplazarReferencia(ref, exp, programa)
//  }
//
//  def reemplazarReferencia(referencia : Referencia,expresion: Expresion, programa: Programa): Unit ={
//    programa.variables(referencia.nombre)= expresion
//  }
//  def buscarReferencia(referencia: Referencia, programa: Programa): Option[Expresion] = referencia match {
//    case Referencia(n) => programa.variables.get(n)
//    case _ => throw new Exception("No es una referencia")
//  }
//
//  def chequearDuplicado(programa: Programa, variable: Variable): Problema ={
//    var problema : Problema = Problema(Ok, "La variable no se encuentra duplicada", variable)
//    if(programa.variables(variable.nombre).equals(variable.valor)){
//      problema = Problema(Advertencia, "variable duplicada", variable)
//    }
//    problema
//  }
//
//  def chequearUsoVarableAntesDeDeclaracion(programa: Programa, referencia: Referencia) : Problema ={
//    var problema : Problema = Problema(Ok, "La variable no se usa antes de su declaracion", referencia)
//    if(programa.variables.contains(referencia.nombre)){
//      problema = Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", referencia)
//    }
//    problema
//  }
//
//  def chequearVariableSinUso(programa: Programa, variable: Variable): Problema = {
//    var problemas : List[Problema] = List()
//    programa.elementos.foreach {
//      case Suma(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Suma(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Resta(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Resta(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Multiplicacion(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Multiplicacion(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Division(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Division(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Mayor(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Mayor(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Menor(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Menor(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Igual(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Igual(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Distinto(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case Distinto(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case MayorOIgual(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case MayorOIgual(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case MenorOIgual(Referencia(variable.nombre), _) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case MenorOIgual(_, Referencia(variable.nombre)) => problemas = problemas :+ Problema(Ok, "La variable es utilizada por este  programa", variable)
//      case _ => Problema(Advertencia, "La variable nunca se utiliza", variable)
//    }
//      if(problemas.map(r=> r.gravedad).contains(Advertencia)){
//        Problema(Ok, "La variable es utilizada por este  programa", variable )
//      }else{
//        Problema(Advertencia, "La variable nunca se utiliza", variable)
//      }
//  }
//
//  def chequearReferenciaValida(referencia: Referencia, programa: Programa): Problema ={
//    var problema : Problema = Problema(Ok, "Es una referencia Valida", referencia)
//    if(!programa.variables.contains(referencia.nombre)){
//      problema = Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", referencia)
//    }
//    problema
//  }
//}
