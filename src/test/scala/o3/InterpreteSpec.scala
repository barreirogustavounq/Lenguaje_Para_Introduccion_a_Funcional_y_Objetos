package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InterpreteSpec extends AnyFunSpec with Matchers {

  describe("Interprete") {
    it("interprete ejecuta operación suma") {
      val interprete = new Interprete
      var operaciones : List[Operacion] = List()
      operaciones = operaciones.appended(Suma(Numero(2), Numero(3)))
      operaciones = operaciones.appended(Resta(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(5))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }

//    it("interprete ejecuta operación resta") {
//      val interprete = new Interprete
//      val programa = Programa(Resta(Numero(10), Numero(6)))
//
//      interprete.ejecutar(programa) should equal(Numero(4))
//    }
//
//    it("interprete ejecuta operación multiplicacion") {
//      val interprete = new Interprete
//      val programa = Programa(Multiplicacion(Numero(2), Numero(3)))
//
//      interprete.ejecutar(programa) should equal(Numero(6))
//    }
//
//    it("interprete ejecuta operación Division") {
//      val interprete = new Interprete
//      val programa = Programa(Division(Numero(10), Numero(2)))
//
//      interprete.ejecutar(programa) should equal(Numero(5))
//    }
  }
}
