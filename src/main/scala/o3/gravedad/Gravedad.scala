package o3.gravedad

abstract class Gravedad()

case class NivelAdvertencia() extends Gravedad {
  override def toString: String = "Advertencia"
}

case class NivelError() extends Gravedad{
  override def toString: String = "Error"
}

case class Ok() extends Gravedad{
  override def toString: String = "Ok"
}
