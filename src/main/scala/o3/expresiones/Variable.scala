package o3.expresiones

import o3.Programa
import o3.gravedad.{Advertencia, Ok, Error}
import o3.problemas.Problema


case class Variable(val nombre : String, var valor : Expresion) extends Expresion


case class Asignar(referencia: Referencia, valor : Expresion) extends Expresion

case class Referencia(nombre : String) extends Expresion

class ExcepcionVariableInexistente extends Exception
class ExcepcionVariableExistente extends Exception

object EliminadorDeVariablesSinUso {

  def optimizar(programa: Programa, analizadorVariable: AnalizadorVariable): Unit ={
    analizadorVariable.analizarVariablesDelaradasSinUso(programa).foreach( v =>
    programa.eliminarVariable(v.nombre))
  }
}
class AnalizadorVariable() {
  def analizarVariablesDuplicadas(programa: Programa): List[Problema]= {
    var respuesta : List[Problema] = List()
    programa.elementos.foreach( e => e match {
      case Variable(nombre, valor) =>
        val rta : Problema = ChequeadorVariable.chequearDuplicado(programa, Variable(nombre, valor))
        respuesta = respuesta.appended(rta)
        rta match {
          case Problema(Ok, _, _) => programa.agregarVariable(Variable(nombre, valor))
          case _ => None
        }
      case _ => None
    })
    respuesta = respuesta.filter(p => p.gravedad != Ok)
    respuesta
  }
  def analizarUsoDeVariablesAntesDeSuDeclaracion(programa: Programa): List[Problema]={
    var respuesta : List[Problema] = List()
    programa.elementos.foreach(e => e match {
      case Variable(nombre, valor) => programa.agregarVariable(Variable(nombre, valor))
      case Referencia(nombre) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Asignar(referencia, valor) => ChequeadorVariable.reemplazarReferencia(referencia, valor, programa)
      case Suma(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Suma(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Resta(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Resta(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Multiplicacion(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Multiplicacion(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Division(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Division(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Mayor(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Mayor(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Menor(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Menor(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Igual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Igual(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Distinto(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case Distinto(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case MayorOIgual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case MayorOIgual(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case MenorOIgual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case MenorOIgual(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearUsoVarableAntesDeDeclaracion(programa, Referencia(nombre)))
      case _ => throw new Exception("No se como analizar la expreseion ")
    })
    respuesta = respuesta.filter(p => p.gravedad != Ok)
    respuesta
  }
  def analizarVariablesDelaradasSinUso(programa: Programa): List[Variable] = {
    var respuesta : List[Variable] = List()
    var referencias : List[Referencia] = List()
    var variablesReferenciadas : List[Variable] = List()
    programa.elementos.foreach(e => e match {
      case Variable(nombre, valor) => if(referencias.map(r => r.nombre).contains(nombre)) {
                                          programa.agregarVariable(Variable(nombre, valor))
                                          variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                      }else{
                                          programa.agregarVariable(Variable(nombre, valor))
                                      }
      case Asignar(referencia, valor) => ChequeadorVariable.reemplazarReferencia(referencia, valor, programa)
      case Referencia(nombre) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Suma(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Suma(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Resta(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Resta(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Multiplicacion(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Multiplicacion(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Division(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Division(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Mayor(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Mayor(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Menor(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Menor(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Igual(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Igual(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Distinto(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case Distinto(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case MayorOIgual(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case MayorOIgual(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case MenorOIgual(Referencia(nombre), _) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case MenorOIgual(_, Referencia(nombre)) => variablesReferenciadas = variablesReferenciadas.appended(ChequeadorVariable.buscarVariable(nombre, programa))
                                                          referencias = referencias.appended(Referencia(nombre))
      case _ => throw new Exception("No se como analizar la expreseion ")
    })

    programa.variables.foreach( v =>
      if(!variablesReferenciadas.contains(v)){
        respuesta = respuesta.appended(v)
      }
      )
        respuesta
  }
  def analizarReferenciaValida(programa: Programa): List[Problema] ={
    var respuesta : List[Problema] = List()
    programa.elementos.foreach(e => e match {
      case Referencia(nombre) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre), programa))
      case Variable(nombre, valor) => if(respuesta.map(r => r.expresion).contains(Referencia(nombre))){
                                        programa.agregarVariable(Variable(nombre, valor))
                                        respuesta = respuesta.filter(p => p.expresion != Referencia(nombre))
                                      }else{
                                        programa.agregarVariable(Variable(nombre, valor))
                                      }
      case Asignar(referencia, valor)=> ChequeadorVariable.asignarReferencia(Asignar(referencia, valor), programa)
      case Suma(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Suma(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Resta(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Resta(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Multiplicacion(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Multiplicacion(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Division(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Division(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Mayor(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Mayor(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Menor(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Menor(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Igual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Igual(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Distinto(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case Distinto(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case MayorOIgual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case MayorOIgual(_, Referencia(nombre)) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case MenorOIgual(Referencia(nombre), _) => respuesta = respuesta.appended(ChequeadorVariable.chequearReferenciaValida(Referencia(nombre),programa))
      case _ => throw new Exception("No se como analizar la expreseion ")
    })
    respuesta = respuesta.filter(p => p.gravedad != Ok)
    respuesta
  }
}

object ChequeadorVariable {

  def asignarReferencia(asignar: Asignar, programa: Programa): Unit = asignar match {
    case Asignar(ref : Referencia, exp : Expresion) => reemplazarReferencia(ref, exp, programa)
  }

  def reemplazarReferencia(referencia : Referencia,expresion: Expresion, programa: Programa): Unit ={
    programa.eliminarVariable(referencia.nombre)
    programa.agregarVariable(Variable(referencia.nombre, expresion))
  }
  def buscarReferencia(referencia: Referencia, programa: Programa): Expresion = {
    programa.variables.filter(v => v.nombre == referencia.nombre).head.valor
  }
  def buscarVariable(nombre : String, programa: Programa) : Variable ={
    val rta : List[Variable] = programa.variables.filter(v => v.nombre == nombre)
    if(!rta.isEmpty){
      rta.head
    }else{
      null
    }
  }

  def chequearDuplicado(programa: Programa, variable: Variable): Problema ={
    var problema : Problema = Problema(Ok, "La variable no se encuentra duplicada", variable)
    if(programa.variables.count(v => v.nombre == variable.nombre) >0){
      problema = Problema(Advertencia, "variable duplicada", variable)
    }
    problema
  }

  def chequearUsoVarableAntesDeDeclaracion(programa: Programa, referencia: Referencia) : Problema ={
    var problema : Problema = Problema(Ok, "La variable no se usa antes de su declaracion", referencia)
    if(comprobarSiUnaVariableContieneUnaReferencia(referencia, programa)){
      problema = Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", referencia)
    }
    problema
  }

  def chequearReferenciaValida(referencia: Referencia, programa: Programa): Problema ={
    var problema : Problema = Problema(Ok, "Es una referencia Valida", referencia)
    if(comprobarSiUnaVariableContieneUnaReferencia(referencia, programa)){
      problema = Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", referencia)
    }
    problema
  }
  def comprobarSiUnaVariableContieneUnaReferencia(referencia: Referencia, programa: Programa): Boolean ={
    !programa.variables.map(v => v.nombre).contains(referencia.nombre)
  }
}
