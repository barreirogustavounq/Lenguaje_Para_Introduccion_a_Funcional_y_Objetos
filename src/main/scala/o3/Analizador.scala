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
    case Suma(Numero(0), Numero(n)) => Respuesta(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", "Suma")
    case Suma(Numero(n), Numero(0)) => Respuesta(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", "Suma")
    case Resta(Numero(n), Numero(0)) => Respuesta(Advertencia, "operacion redundante: restar 0 retorna el mismo numero", "Resta")
    case Multiplicacion(Numero(1), Numero(n)) => Respuesta(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero ", "Multiplicacion")
    case Multiplicacion(Numero(n), Numero(1)) => Respuesta(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", "Multiplicacion")
    case Division(Numero(n), Numero(1)) => Respuesta(Advertencia, "operacion redundante: dividir por 1 retorna el mismo numero", "Division")
    case Mayor(Numero(n1), Numero(n2)) if (n1 > n2) => Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion Mayor")
    case Mayor(Numero(n1), Numero(n2)) if (!(n1 > n2)) => Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion Mayor")
    case Menor(Numero(n1), Numero(n2)) if (n1 < n2) => Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion Menor")
    case Menor(Numero(n1), Numero(n2)) if !(n1 < n2) => Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion Menor")
    case Igual(Numero(n1), Numero(n2)) if (n1 == n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion Igual")
    case Igual(Numero(n1), Numero(n2)) if !(n1 == n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion Igual")
    case Distinto(Numero(n1), Numero(n2)) if (n1 != n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion Distinto")
    case Distinto(Numero(n1), Numero(n2)) if !(n1 != n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion Distinto")
    case MenorOIgual(Numero(n1), Numero(n2)) if (n1 >= n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion MenorIgual")
    case MenorOIgual(Numero(n1), Numero(n2)) if !(n1 >= n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion MenorIgual")
    case MayorOIgual(Numero(n1), Numero(n2)) if (n1 <= n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna true", "Comparacion mayorIgual")
    case MayorOIgual(Numero(n1), Numero(n2)) if !(n1 <= n2)=> Respuesta(Advertencia, "operacion redundante: siempre returna false", "Comparacion mayorIgual")
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