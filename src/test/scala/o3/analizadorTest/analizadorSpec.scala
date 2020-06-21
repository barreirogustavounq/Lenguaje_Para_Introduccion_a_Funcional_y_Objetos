package o3.analizadorTest

import o3.Programa
import o3.expresiones._
import o3.gravedad.Advertencia
import o3.motores.Analizador
import o3.problemas.Problema
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class analizadorSpec extends AnyFunSpec with Matchers {

  describe("Analizador") {
    it("Analiza las operaciones Suma, Resta, Multiplicacion y Division ") {
      val analizador = new Analizador
      var operaciones : List[Operacion] = List()
      operaciones = operaciones :+ Suma(Numero(2), Numero(0))
      operaciones = operaciones :+ Resta(Numero(4), Numero(0))
      operaciones = operaciones :+ Multiplicacion(Numero(1), Numero(4))
      operaciones = operaciones :+ Division(Numero(4), Numero(0))
      operaciones = operaciones :+ Division(Numero(4), Numero(1))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = analizador.analizar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Problema(Advertencia, "operacion redundante: sumar 0 retorna el mismo numero", Suma(Numero(2), Numero(0))))
      resultadoDeEjecutarPrograma(1) should equal(Problema(Advertencia, "operacion redundante: restar 0 retorna el mismo numero", Resta(Numero(4), Numero(0))))
      resultadoDeEjecutarPrograma(2) should equal(Problema(Advertencia, "operacion redundante: multiplicar por 1 retorna el mismo numero", Multiplicacion(Numero(1), Numero(4))))
      resultadoDeEjecutarPrograma(3) should equal(Problema(Advertencia, "No se puede dividir por cero", Division(Numero(4), Numero(0))))
      resultadoDeEjecutarPrograma(4) should equal(Problema(Advertencia, "operacion redundante: dividir por 1 retorna el mismo numero", Division(Numero(4), Numero(1))))
    }
    it("Analiza las operaciones Mayor, Menor, Igual, Distinto, MayorOigual y MenorOIgual") {
      val analizador = new Analizador
      var operaciones : List[Operacion] = List()
      operaciones = operaciones :+ Mayor(Numero(2), Numero(0))
      operaciones = operaciones :+ Menor(Numero(4), Numero(2))
      operaciones = operaciones :+ Igual(Numero(1), Numero(4))
      operaciones = operaciones :+ Distinto(Numero(4), Numero(0))
      operaciones = operaciones :+ MayorOIgual(Numero(4), Numero(1))
      operaciones = operaciones :+ MenorOIgual(Numero(1), Numero(1))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = analizador.analizar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna true", Mayor(Numero(2), Numero(0))))
      resultadoDeEjecutarPrograma(1) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna false", Menor(Numero(4), Numero(2))))
      resultadoDeEjecutarPrograma(2) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna false", Igual(Numero(1), Numero(4))))
      resultadoDeEjecutarPrograma(3) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna true", Distinto(Numero(4), Numero(0))))
      resultadoDeEjecutarPrograma(4) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna true", MayorOIgual(Numero(4), Numero(1))))
      resultadoDeEjecutarPrograma(5) should equal(Problema(Advertencia, "comparación sin sentido: siempre retorna true", MenorOIgual(Numero(1), Numero(1))))
    }
  }
}
