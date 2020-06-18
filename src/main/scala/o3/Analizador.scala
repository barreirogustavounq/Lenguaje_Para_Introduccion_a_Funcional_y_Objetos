package o3

class Analizador {


  def analizarOperacion(operacion: Operacion) : List[Respuesta] = {
    var respuestas : List[Respuesta] = List()
    Reglamento.reglas.foreach { r =>
      respuestas = respuestas.appended(r.aplicarRegla(operacion))
    }
    respuestas
  }

  def analizar(programa: Programa) : List[List[Respuesta]] = {
    val myList = programa.elementos.map(op => analizarOperacion(op))
    println(myList)
    myList
  }
}


/***************DIFERENTES TIPOS DE GRAVEDAD PARA EL ANALISIS******************/
class Gravedad()
object Ok extends Gravedad
object Advertencia extends Gravedad
object Error extends Gravedad

/**************************MOTOR DE REGLAS*************************************/
trait Regla {
  def aplicarRegla(operacion: Operacion): Respuesta
}

/********************* REGLAS DEFINIDAS EN LA CONSIGNA ***********************/
object DividirPorCero extends Regla{
  override def aplicarRegla(operacion: Operacion): Respuesta = operacion match {
    case Division(Numero(_), Numero(0)) => Respuesta(Error, "No se puede dividir por cero", operacion.getClass.getSimpleName)
    case _ => Respuesta(Ok, "no hay problemas en la operacion", operacion.getClass.getSimpleName)
  }
}

object OperacionRedundante extends Regla {

  override def aplicarRegla(operacion: Operacion): Respuesta = operacion match {
    case Suma(Numero(0), Numero(n)) => Respuesta(Advertencia, "operacion redundante", "Suma")
    case Suma(Numero(n), Numero(0)) => Respuesta(Advertencia, "operacion redundante", "Suma")
    case Resta(Numero(n), Numero(0)) => Respuesta(Advertencia, "operacion redundante", "Resta")
    case Multiplicacion(Numero(1), Numero(n)) => Respuesta(Advertencia, "operacion redundante", "Multiplicacion")
    case Multiplicacion(Numero(n), Numero(1)) => Respuesta(Advertencia, "operacion redundante", "Multiplicacion")
    case Division(Numero(n), Numero(1)) => Respuesta(Advertencia, "operacion redundante", "Division")
    case _ => Respuesta(Ok, "no hay problemas en la operacion", operacion.getClass.getSimpleName)
  }
}

/****************REGLAMENTO******************************/

object  Reglamento {
  var reglas : List[Regla] = List(DividirPorCero, OperacionRedundante)

  def agregarRegla(regla : Regla): Unit ={
    reglas = reglas.appended(regla)
  }
}

/******************RESPUESTA***************************************************/
case class Respuesta(gravedad : Gravedad, mensaje : String, nombreOPeracion : String){

}