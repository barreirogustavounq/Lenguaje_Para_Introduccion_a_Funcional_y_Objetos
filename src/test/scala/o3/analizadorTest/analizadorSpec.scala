package o3.analizadorTest

import o3.Programa
import o3.expresiones._
import o3.gravedad.{Advertencia, Error}
import o3.motores.{Analizador}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class analizadorSpec extends AnyFunSpec with Matchers {

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
