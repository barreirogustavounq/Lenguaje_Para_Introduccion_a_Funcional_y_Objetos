package o3.expresiones

import o3.Programa
import o3.gravedad.{Advertencia, Ok}
import o3.respuesta.Respuesta

case class Variable(nombre : String, numero : Numero) extends Expresion

case class Asignar(referencia : Referencia, numero : Numero)
case class Referencia(nombre : String) extends Expresion {
  var variables : Map[String, Expresion] = Map()

}

class AnalizadorVariable {



}

object Chequeo {

  def buscarReferencia(referencia: Referencia): Option[Expresion] = referencia match {
    case Referencia(n) => referencia.variables.get(n)
    case _ => throw new Exception("No es una referencia")
  }

  def chequearDuplicado(programa: Programa, variable: Variable): Respuesta ={
    var respuesta : Respuesta = Respuesta(Ok, "La variable no se encuentra duplicada", "variable" )
    if(programa.variables.map(v=> v.nombre).contains(variable.nombre)){
      respuesta = Respuesta(Advertencia, "variable duplicada", "variable")
    }
    respuesta
  }

  def chequearUsoVarableAntesDeDeclaracion(programa: Programa, referencia: Referencia) : Respuesta ={
    var respuesta : Respuesta = Respuesta(Ok, "La variable no se usa antes de su declaracion", "variable" )
    if(programa.variables.map(v=> v.nombre).contains(referencia.nombre)){
      respuesta = Respuesta(Advertencia, "Esta referencia no se encuentra definida en una variable", "variable")
    }
    respuesta
  }

  def chequearVariableSinUso(programa: Programa, variable: Variable): Respuesta = {
    var respuestas : List[Respuesta] = List()
    programa.elementos.foreach( e => e match {
      case Suma(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Suma(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Resta(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Resta(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Multiplicacion(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Multiplicacion(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Division(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Division(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Mayor(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Mayor(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Menor(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Menor(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Igual(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Igual(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Distinto(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case Distinto(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case MayorOIgual(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case MayorOIgual(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case MenorOIgual(Referencia(variable.nombre),_) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case MenorOIgual(_, Referencia(variable.nombre)) => respuestas = respuestas.appended(Respuesta(Ok, "La variable es utilizada este  programa", "variable" ))
      case _ => Respuesta(Advertencia, "La variable nunca se usa", "variable")
    })
    Respuesta(Advertencia, "","")
  }
}