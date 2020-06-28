package o3.motores

import o3.excepciones.{ExcepcionVariableNoAsignada, ExcepcionVariableNoDeclarada}
import o3.expresiones.{Referencia, Valor}

case class Contexto(var referencias: Map[String, Option[Valor]] = Map()) {
  def asignar(referencia: Referencia, valor: Valor) = {
    referencias.get(referencia.nombre) match {
      case None => throw new ExcepcionVariableNoDeclarada
      case Some(_) => referencias += (referencia.nombre -> Some(valor))
    }
  }

  def declarar(nombre: String, valor: Option[Valor]) = {
    referencias += (nombre -> valor)
  }

  def obtener(referencia: Referencia): Valor =
    referencias.get(referencia.nombre) match {
      case None => throw new ExcepcionVariableNoDeclarada
      case Some(None) => throw new ExcepcionVariableNoAsignada
      case Some(Some(v: Valor)) => v
    }
}
