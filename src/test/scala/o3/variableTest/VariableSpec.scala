package o3.variableTest

import o3.expresiones._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class VariableSpec extends AnyFunSpec with Matchers {

  describe("Variables") {
    it("Excepcion al intentar crear variable con nombre existente") {
      Variable("edad", Numero(27))

      try Variable("edad", Numero(28))
      catch {case e : ExcepcionVariableExistente => true}
    }

    it("Excepcion al intentar usar variable antes de su declarancion") {
      try Referencia("noInicializada")
      catch {case e : ExcepcionVariableInexistente => true}
    }

    it("Creo una variable y lka llamo por la referencia") {
      Variable("anioActual", Numero(2020))
      Variable("edad", Numero(27))
      Variable("anioNacimiento", Resta(Referencia("anioActual"), Referencia("edad")))
      Referencia("anioNacimiento") should equal(Resta(Numero(2020),Numero(27)))
    }
  }
}
