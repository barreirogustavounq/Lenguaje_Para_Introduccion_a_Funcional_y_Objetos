package o3.variables

import o3.SentenciaCompuesta
import o3.expresiones.{Nulo, Referencia, Valor}
import o3.motores.Contexto

case class Variable(nombre: String, _valor: Valor = null) extends SentenciaCompuesta {
  var valor: Option[Valor] = Option(_valor)

  override def ejecutar(contexto: Contexto): Valor = {
    valor match {
      case Some(v) => contexto.declarar(nombre, Some(v.ejecutar(contexto)))
      case None => contexto.declarar(nombre, None)
    }
    Nulo()
  }
}

case class Asignar(referencia: Referencia, valor: Valor) extends SentenciaCompuesta {
  sentenciasHijas = valor :: referencia :: sentenciasHijas

  override def ejecutar(contexto: Contexto): Valor = {
    contexto.asignar(referencia, valor.ejecutar(contexto))
    Nulo()
  }
}
