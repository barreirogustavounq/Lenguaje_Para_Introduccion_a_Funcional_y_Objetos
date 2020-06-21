package o3.problemas
import o3.expresiones.Expresion
import o3.gravedad.Gravedad

case class Problema(gravedad : Gravedad, descripcion : String, expresion: Expresion) {
  override def equals(that : Any) : Boolean ={
    that match{
      case that : Problema => that.descripcion == descripcion
      case _ => false
    }
  }
}
