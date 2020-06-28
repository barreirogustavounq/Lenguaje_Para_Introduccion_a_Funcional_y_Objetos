package o3.problemas
import o3.Sentencia
import o3.gravedad.{Gravedad, NivelAdvertencia, NivelError}
import o3.reglamento.Regla

class Problema(val regla: Regla, val gravedad: Gravedad, val sentencia: Sentencia) {
  override def toString: String = {
    s"$gravedad | $descripcion -> $sentencia"
  }

  def descripcion: String = regla.mensaje
}

case class Error(_regla: Regla, s: Sentencia) extends Problema(_regla, NivelError(), s)

case class Advertencia(_regla: Regla, s: Sentencia) extends Problema(_regla, NivelAdvertencia(), s)

object ProblemaConRegla {
  def unapply(arg: Problema): Option[(Regla, Sentencia)] = Some(arg.regla, arg.sentencia)
}
