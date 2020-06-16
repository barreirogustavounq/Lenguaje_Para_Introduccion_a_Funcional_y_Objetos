package o3

class Analizador {

  def analizarOperacion(operacion: Operacion) : Respuesta = operacion match {
    case  Suma(Numero(n1), Numero(n2)) => chequearSuma(operacion)
    case  Resta(Numero(n1), Numero(n2)) => chequearResta(operacion)
    case  Multiplicacion(Numero(n1), Numero(n2)) => chequearMultiplicacion(operacion)
    case  Division(Numero(n1), Numero(n2)) => chequearDivision(operacion)
  }
  def chequearSuma(operacion: Operacion) : Respuesta = operacion match {
    case Suma(Numero(0), Numero(n2)) => new Respuesta(Advertencia, "Todo numero sumado a cero es el mismo numero", Numero(n2))
    case Suma(Numero(n1), Numero(0)) => new Respuesta(Advertencia, "Todo numero sumado a cero es el mismo numero", Numero(n1))
  }
  def chequearResta(operacion: Operacion) : Respuesta = operacion match {
    case Resta(Numero(n1), Numero(0)) => new Respuesta(Advertencia, "Todo numero que es restado por cero es el mismo numero", Numero(n1))
  }
  def chequearMultiplicacion(operacion: Operacion) : Respuesta = operacion match {
    case Multiplicacion(Numero(n1), Numero(0)) => new Respuesta(Advertencia, "Todo numero que es multiplicado por cero el resultado sera 0", Numero(0))
    case Multiplicacion(Numero(0), Numero(n2)) => new Respuesta(Advertencia, "Al multiplicar 0 por cualquier otro numero el resultado sera 0", Numero(0))
    case Multiplicacion(Numero(1), Numero(n2)) => new Respuesta(Advertencia, "Todo numero que es multiplicado por uno es el mismo numero", Numero(n2))
    case Multiplicacion(Numero(n1), Numero(1)) => new Respuesta(Advertencia, "Todo numero que es multiplicado por uno es el mismo numero", Numero(n1))
  }
  def chequearDivision(operacion: Operacion) : Respuesta = operacion match {
    case Division(Numero(n1), Numero(0)) => new Respuesta(Error, "es imposible dividir por cero", throw new UnsupportedOperationException("es imposible dividir por cero"))
    case Division(Numero(0), Numero(n2)) => new Respuesta(Advertencia, "Al dividir al numero cero por cualquier otro numero el resultado sera cero", Numero(0))
    case Division(Numero(n1), Numero(1)) => new Respuesta(Advertencia, "Todo numero que es dividido por uno es el mismo numero", Numero(n1))
  }

  def analizar(programa: Programa) : List[Respuesta] = {
    var myList = programa.elementos.map(op => analizarOperacion(op))
    return myList
  }
}

case class Respuesta(gravedad : Gravedad, mensaje : String, optimizacion : Numero)

case class Gravedad()
object Advertencia extends Gravedad
object Error extends Gravedad

