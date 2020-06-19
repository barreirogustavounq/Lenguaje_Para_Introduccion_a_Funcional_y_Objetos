package o3

import o3.expresiones.{Booleano, Distinto, Division, Expresion, Igual, Mayor, MayorOIgual, Menor, MenorOIgual, Multiplicacion, Numero, Operacion, Resta, Suma}
import o3.gravedad.{Advertencia, Error}
import o3.motores.{Analizador, Interprete}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InterpreteSpec extends AnyFunSpec with Matchers {

  describe("Interprete") {
    it("interprete ejecuta operación suma") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      operaciones = operaciones.appended(Suma(Numero(2), Numero(3)))
      operaciones = operaciones.appended(Suma(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(5))
      resultadoDeEjecutarPrograma(1) should equal(Numero(6))
    }
    it("interprete ejecuta operación resta") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      operaciones = operaciones.appended(Resta(Numero(7), Numero(3)))
      operaciones = operaciones.appended(Resta(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(4))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }
    it("interprete ejecuta operación Multuplicacion") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      operaciones = operaciones.appended(Multiplicacion(Numero(7), Numero(3)))
      operaciones = operaciones.appended(Multiplicacion(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(21))
      resultadoDeEjecutarPrograma(1) should equal(Numero(8))
    }
    it("interprete ejecuta operación Division") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      operaciones = operaciones.appended(Division(Numero(12), Numero(3)))
      operaciones = operaciones.appended(Division(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(4))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }
    it("interprete ejecuta operación Mayor, Menor, Igual, Distinto, MayorOIgual, MenorOIgual") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      operaciones = operaciones.appended(Mayor(Numero(12), Numero(3)))
      operaciones = operaciones.appended(Mayor(Numero(4), Numero(8)))
      operaciones = operaciones.appended(Menor(Numero(12), Numero(15)))
      operaciones = operaciones.appended(Menor(Numero(4), Numero(1)))
      operaciones = operaciones.appended(Igual(Numero(12), Numero(12)))
      operaciones = operaciones.appended(Igual(Numero(4), Numero(1)))
      operaciones = operaciones.appended(Distinto(Numero(11), Numero(12)))
      operaciones = operaciones.appended(Distinto(Numero(4), Numero(4)))
      operaciones = operaciones.appended(MayorOIgual(Numero(12), Numero(12)))
      operaciones = operaciones.appended(MayorOIgual(Numero(12), Numero(11)))
      operaciones = operaciones.appended(MayorOIgual(Numero(2), Numero(6)))
      operaciones = operaciones.appended(MenorOIgual(Numero(11), Numero(11)))
      operaciones = operaciones.appended(MenorOIgual(Numero(1), Numero(11)))
      operaciones = operaciones.appended(MenorOIgual(Numero(28), Numero(6)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(1) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(2) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(3) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(4) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(5) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(6) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(7) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(8) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(9) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(10) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(11) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(12) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(13) should equal(Booleano(false))
    }
    it("interprete ejecuta operación desconocida") {
      val interprete = new Interprete
      var operaciones: List[Operacion] = List()
      case class Desconocido(n1: Numero, n2: Numero) extends Operacion(n1, n2)
      operaciones = operaciones.appended(Desconocido(Numero(12), Numero(4)))
      val programa = Programa(operaciones)
      assertThrows[UnsupportedOperationException] {
       interprete.ejecutar(programa)
      }
    }
  }
  describe("Analizador") {
    it("Analiza las operaciones Suma, Resta, Multiplicacion y Division ") {
      val analizador = new Analizador
      var operaciones : List[Operacion] = List()
      operaciones = operaciones.appended(Suma(Numero(2), Numero(0)))
      operaciones = operaciones.appended(Resta(Numero(4), Numero(0)))
      operaciones = operaciones.appended(Multiplicacion(Numero(1), Numero(4)))
      operaciones = operaciones.appended(Division(Numero(4), Numero(0)))
      operaciones = operaciones.appended(Division(Numero(4), Numero(1)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = analizador.analizar(programa).map(listaRespuestas => listaRespuestas.map(r => r.gravedad))
      resultadoDeEjecutarPrograma(0).contains(Advertencia) should equal(true)
      resultadoDeEjecutarPrograma(1).contains(Advertencia) should equal(true)
      resultadoDeEjecutarPrograma(2).contains(Advertencia) should equal(true)
      resultadoDeEjecutarPrograma(3).contains(Error) should equal(true)
      resultadoDeEjecutarPrograma(4).contains(Advertencia) should equal(true)
    }
    it("Analiza las operaciones Mayor, Menor, Igual, Distinto, MayorOigual y MenorOIgual") {
      val analizador = new Analizador
      var operaciones : List[Operacion] = List()
      operaciones = operaciones.appended(Mayor(Numero(2), Numero(0)))
      operaciones = operaciones.appended(Menor(Numero(4), Numero(2)))
      operaciones = operaciones.appended(Igual(Numero(1), Numero(4)))
      operaciones = operaciones.appended(Distinto(Numero(4), Numero(0)))
      operaciones = operaciones.appended(MayorOIgual(Numero(4), Numero(1)))
      operaciones = operaciones.appended(MenorOIgual(Numero(1), Numero(1)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = analizador.analizar(programa).map(listaRespuestas => listaRespuestas.map(r => r.descripcion))
      resultadoDeEjecutarPrograma(0).contains("operacion redundante: siempre returna true") should equal(true)
      resultadoDeEjecutarPrograma(1).contains("operacion redundante: siempre returna false") should equal(true)
      resultadoDeEjecutarPrograma(2).contains("operacion redundante: siempre returna false") should equal(true)
      resultadoDeEjecutarPrograma(3).contains("operacion redundante: siempre returna true") should equal(true)
      resultadoDeEjecutarPrograma(4).contains("operacion redundante: siempre returna true") should equal(true)
      resultadoDeEjecutarPrograma(5).contains("operacion redundante: siempre returna true") should equal(true)
    }
  }
}
