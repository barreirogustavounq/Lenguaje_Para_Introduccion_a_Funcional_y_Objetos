package o3.optimizadorTest

import o3.Programa
import o3.expresiones._
import o3.motores.{Optimizador}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class optmizadorSpec extends AnyFunSpec with Matchers {

  describe("Optimizador") {
    it("Optimiza las operaciones Mayor, Menor, Igual, Distinto, MayorOIgual y MenorOigual") {
      val optimizador = new Optimizador
      var operaciones : List[Expresion] = List()
      operaciones = operaciones :+ Mayor(Numero(2), Numero(0))
      operaciones = operaciones :+ Menor(Numero(4), Numero(2))
      operaciones = operaciones :+ Igual(Numero(1), Numero(4))
      operaciones = operaciones :+ Distinto(Numero(4), Numero(0))
      operaciones = operaciones :+ MayorOIgual(Numero(4), Numero(1))
      operaciones = operaciones :+ MenorOIgual(Numero(1), Numero(1))
      val programa = Programa(operaciones)
      optimizador.optimizarPrograma(programa)
      programa.elementos(0) should equal(Booleano(true))
      programa.elementos(1) should equal(Booleano(false))
      programa.elementos(2) should equal(Booleano(false))
      programa.elementos(3) should equal(Booleano(true))
      programa.elementos(4) should equal(Booleano(true))
      programa.elementos(5) should equal(Booleano(true))
    }
    it("Optimiza las operaciones Suma, Resta, Multiplicacion y Division  ") {
      val optimizador = new Optimizador
      var operaciones : List[Expresion] = List()
      operaciones = operaciones :+ Suma(Numero(2), Numero(0))
      operaciones = operaciones :+ Resta(Numero(4), Numero(0))
      operaciones = operaciones :+ Multiplicacion(Numero(1), Numero(4))
      operaciones = operaciones :+ Division(Numero(4), Numero(0))
      operaciones = operaciones :+ Division(Numero(4), Numero(1))
      val programa = Programa(operaciones)
      optimizador.optimizarPrograma(programa)
      programa.elementos(0) should equal(Numero(2))
      programa.elementos(1) should equal(Numero(4))
      programa.elementos(2) should equal(Numero(4))
      programa.elementos(3) should equal(Division(Numero(4), Numero(0)))
      programa.elementos(4) should equal(Numero(4))
    }
  }
}
