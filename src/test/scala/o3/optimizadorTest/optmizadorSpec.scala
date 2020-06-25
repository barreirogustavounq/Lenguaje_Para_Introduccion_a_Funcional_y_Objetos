package o3.optimizadorTest

import o3.Programa
import o3.ReglasPredefinidas.{ComparacionesSinSentido, DividicionPorCero, OperacionRedundante}
import o3.expresiones._
import o3.motores.Optimizador
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class optmizadorSpec extends AnyFunSpec with Matchers {

  describe("Optimizador") {
    it("Optimiza las operaciones Mayor, Menor, Igual, Distinto, MayorOIgual y MenorOigual") {
      val optimizador = new Optimizador
      val operaciones : List[Expresion] = List(
        Mayor(Numero(2), Numero(0)),
        Menor(Numero(4), Numero(2)),
        Igual(Numero(1), Numero(4)),
        Distinto(Numero(4), Numero(0)),
        MayorOIgual(Numero(4), Numero(1)),
        MenorOIgual(Numero(1), Numero(1))
      )
      val programa = Programa(operaciones)
      optimizador.optimizarPrograma(programa, List(ComparacionesSinSentido))
      programa.elementos(0) should equal(True)
      programa.elementos(1) should equal(False)
      programa.elementos(2) should equal(False)
      programa.elementos(3) should equal(True)
      programa.elementos(4) should equal(True)
      programa.elementos(5) should equal(True)
    }
    it("Optimiza las operaciones Suma, Resta, Multiplicacion y Division  ") {
      val optimizador = new Optimizador
      val operaciones : List[Expresion] = List(
        Suma(Numero(2), Numero(0)),
        Resta(Numero(4), Numero(0)),
        Multiplicacion(Numero(1), Numero(4)),
        Division(Numero(4), Numero(0)),
        Division(Numero(4), Numero(1))
      )
      val programa = Programa(operaciones)
      optimizador.optimizarPrograma(programa, List(DividicionPorCero, OperacionRedundante))
      programa.elementos(0) should equal(Numero(2))
      programa.elementos(1) should equal(Numero(4))
      programa.elementos(2) should equal(Numero(4))
      programa.elementos(3) should equal(Division(Numero(4), Numero(0)))
      programa.elementos(4) should equal(Numero(4))
    }
  }
}
