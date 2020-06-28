package o3.analizadorTest

import o3.Programa
import o3.ReglasPredefinidas.{ComparacionesSinSentido, ReglaNoMultiplicarPorUno, ReglaNoRestarCero, ReglaNoSumarCero}
import o3.expresiones._
import o3.motores.Analizador
import o3.reglamento.Regla
import o3.variables._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class analizadorSpec extends AnyFunSpec with Matchers {

  val otroNumero: Numero = Numero(1)

  def AnalizadorCon(reglas: Regla*): Analizador = {
    Analizador(reglas.toList)
  }

  describe("Analizador") {
    it("ReglaNoSumarCero: detectar si un operando es 0") {
      val p = Programa(
        Suma(Numero(0), otroNumero),
        Suma(otroNumero, Numero(0)),
        Suma(Numero(1), Numero(1))
      )
      val problemas = AnalizadorCon(ReglaNoSumarCero()).analizar(p)
      problemas.size should be(2)
      problemas.head.toString should equal("Advertencia | No sumar cero -> Suma(Numero(0),Numero(1))")
      problemas(1).toString should equal("Advertencia | No sumar cero -> Suma(Numero(1),Numero(0))")
    }

    it("ReglaNoRestarCero: detectar si el segundo operando es 0") {
      val p = Programa(
        Resta(Numero(0), otroNumero),
        Resta(otroNumero, Numero(0)),
        Resta(Numero(1), Numero(1))
      )
      val problemas = AnalizadorCon(ReglaNoRestarCero()).analizar(p)
      problemas.size should be(1)
      problemas.head.toString should equal("Advertencia | No restar cero -> Resta(Numero(1),Numero(0))")
    }

    it("ReglaNoMultiplicarPorUno: detectar si un operando es 1") {
      val p = Programa(
        Multiplicacion(Numero(1), otroNumero),
        Multiplicacion(otroNumero, Numero(1)),
        Multiplicacion(Numero(2), Numero(2))
      )
      val problemas = AnalizadorCon(ReglaNoMultiplicarPorUno()).analizar(p)
      problemas.size should be(2)
      problemas.head.toString should equal("Advertencia | No multiplicar por 1 -> Multiplicacion(Numero(1),Numero(1))")
    }

    it("ComparacionesSinSentido: detectar 1 == 1") {
      val p = Programa(Igual(Numero(1), Numero(1)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)

    }
    it("detectar 1 != 1") {
      val p = Programa(Distinto(Numero(1), Numero(1)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)
    }
    it("detectar 2 > 1") {
      val p = Programa(Mayor(Numero(2), Numero(1)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)
    }
    it("detectar 2 >= 1") {
      val p = Programa(MayorOIgual(Numero(2), Numero(1)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)
    }
    it("detectar 1 < 3") {
      val p = Programa(Menor(Numero(1), Numero(3)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)
    }
    it("detectar 1 <= 3") {
      val p = Programa(MenorOIgual(Numero(1), Numero(3)))
      AnalizadorCon(ComparacionesSinSentido()).analizar(p).size should be(1)
    }

    it("ReglaVariableDuplicada:  detectar una variable duplicada") {
      val s = Variable("a")
      val p = Programa(s, s)
      val res = AnalizadorCon(ReglaVariableDuplicada()).analizar(p)
      res.size should be(2)
      assert(res.head.sentencia == s)
    }

    it("ReglaVariableDuplicada:  no detectar una variable no duplicada") {
      val p = Programa(Variable("a"))
      AnalizadorCon(ReglaVariableDuplicada()).analizar(p).size should be(0)
    }

    it("ReglaReferenciaAVariableNoDeclarada:  detectar referencia una variable no declarada") {
      val r = Referencia("a")
      val p = Programa(r, Variable("a"))
      val res = AnalizadorCon(ReglaReferenciaAVariableNoDeclarada()).analizar(p)
      res.size should be(1)
      assert(res.head.sentencia == r)
    }

    it("no detectar referencia a una variable declarada") {
      val p = Programa(Referencia("a"), Variable("b"), Referencia("b"))
      AnalizadorCon(ReglaReferenciaAVariableNoDeclarada()).analizar(p).size should be(1)
    }

    it("detectar referencia a una variable no declarada dentro de una operacion") {
      val s = Referencia("b")
      val p = Programa(Suma(Referencia("b"), Numero(0)), Variable("b", Numero(2)))
       val res = AnalizadorCon(ReglaReferenciaAVariableNoDeclarada()).analizar(p)
      res.size should be(1)
      assert(res.head.sentencia == s)
    }

    it("ReglaVariableDeclaradaSinUso: detectar una variable sin uso") {
      val s = Variable("b")
      val p = Programa(Suma(Numero(4), Numero(0)), s)
      val res = AnalizadorCon(ReglaVariableDeclaradaSinUso()).analizar(p)
      res.size should be(1)
      assert(res.head.sentencia == s)
    }

    it("no detectar una variable en uso") {
      val s = Variable("b")
      val p = Programa(Suma(Numero(4), Numero(0)), s, Asignar(Referencia("b"), Numero(4)))
      AnalizadorCon(ReglaVariableDeclaradaSinUso()).analizar(p).size should be(0)
    }

    it("ReglaNoSumarCero y ComparacionesSinSentido: detectar ambas cosas") {
      val p = Programa(
        Suma(Numero(0), otroNumero),
        Suma(otroNumero, Numero(0)),
        Suma(Numero(1), Numero(1)),
        Igual(Numero(1), Numero(1))
      )
      val problemas = AnalizadorCon(ReglaNoSumarCero(), ComparacionesSinSentido()).analizar(p)
      problemas.size should be(3)
    }
  }
}
