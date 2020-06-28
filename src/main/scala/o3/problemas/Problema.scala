package o3.problemas
import o3.Sentencia
import o3.gravedad.{Gravedad, NivelAdvertencia, NivelError}
import o3.reglamento.Regla

class Problema(val regla: Regla, val gravedad: Gravedad, val sentencia: Sentencia) {
  override def toString: String = {
    s"$gravedad | $descripcion -> $sentencia"
  }

  def descripcion = regla.mensaje
}

case class Error(_regla: Regla, sent: Sentencia) extends Problema(_regla, NivelError(), sent)

case class Advertencia(_regla: Regla, sent: Sentencia) extends Problema(_regla, NivelAdvertencia(), sent)

object ProblemaConRegla {
  def unapply(arg: Problema): Option[(Regla, Sentencia)] = Some(arg.regla, arg.sentencia)
}
