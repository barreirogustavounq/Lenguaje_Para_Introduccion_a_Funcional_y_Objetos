package o3.interpreteTest

import o3.Programa
import o3.expresiones.{Division, Multiplicacion, Nulo, Numero, Referencia, Resta, Suma, Valor}
import o3.motores.Interprete
import o3.variables.Variable
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InterpreteSpec extends AnyFunSpec with Matchers {

  describe("Interprete") {
    val n4= Numero(4)
    val n3= Numero(3)

    it("ejecutar 3+4 y devolver 7") {
      val p = Programa(Suma(n3, n4))
      val r: Valor = Interprete().ejecutar(p)
      r should be (Numero(7))
    }

    it("ejecutar Variable('var') y devolver Nulo") {
      val p = Programa(Variable("var"))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Nulo())
    }

    it("ejecutar Variable('var',9-1) y devolver Nulo") {
      val p = Programa(Variable("var", Resta(Numero(9), Numero(1))))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Nulo())
    }

    it("ejecutar (9-1)-2 y devolver 6") {
      val p = Programa(Resta(Resta(Numero(9), Numero(1)), Numero(2)))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Numero(6))
    }

    it("ejecutar 9/3 y devolver 3") {
      val p = Programa(Division(Numero(9), Numero(3)))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Numero(3))
    }

    it("ejecutar 2*4 y devolver 8") {
      val p = Programa(Multiplicacion(Numero(2), Numero(4)))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Numero(8))
    }

    it("ejecutar var=1; -7+var; y devolver -6") {
      val p = Programa(Variable("var", Numero(1)), Suma(Numero(-7), Referencia("var")))
      val r: Valor = Interprete().ejecutar(p)
      r should be(Numero(-6))
    }
  }
}
